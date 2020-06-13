module arc.memory;

import core.memory : pageSize;

size_t kib(size_t n) {
    return n * 1024;
}

size_t mib(size_t n) {
    return n * (1024 * 2);
}

size_t gib(size_t n) {
    return n * (1024 ^ 3);
}

size_t round_to_page(size_t n) {
    return n + (pageSize - n % pageSize);
}

/**
 This struct provides an interface for reserving a block of virtual memory, then
 allocating from it sequentially. It returns all of its memory upon destruction.
 */
struct VirtualMemory {
    import std.algorithm : min, max;

    enum standard_alignment = 8;

    version (Windows) {
        import core.sys.windows.winbase : VirtualAlloc, VirtualFree;
        import core.sys.windows.winnt : MEM_RELEASE, MEM_COMMIT, MEM_RESERVE,
            PAGE_READWRITE, PAGE_NOACCESS;
    }

public:
    this(size_t size_bytes) {
        _extra_bytes_per_alloc = extra_pages_per_alloc * pageSize;
        _num_bytes_reserved = round_to_page(size_bytes);

        version (Windows) {
            _region_start = VirtualAlloc(null, _num_bytes_reserved, MEM_RESERVE, PAGE_NOACCESS);
            if (!_region_start)
                assert(0, "Failed to allocate memory.");

            _region_end = _next_alloc_start = _region_start;
        }
        else
            static assert(false, "Platform not supported.");
    }

    @disable this(this);

    ~this() {
        version (Windows) {
            const status = VirtualFree(_region_start, 0, MEM_RELEASE);
            assert(status, "Failed to free memory.");
        }
        else
            static assert(false, "Platform not supported for IndexedRegion.");
    }

    /**
     Allocates n bytes of memory.

     For simplicity, this function will always return memory aligned to the
     standard alignment, which is described at the top of the struct.
     */
    void[] alloc(size_t n)
    in(alloc_size(n, standard_alignment) <= capacity) {
        const alloc_size = alloc_size(n, standard_alignment);
        const alloc_diff = alloc_size - n; // This subtraction could be avoided by unpacking alloc_size
        auto next_after_alloc = _next_alloc_start + alloc_size;

        if (next_after_alloc > _region_end)
            reserve(next_after_alloc - _region_end);

        auto mem = _next_alloc_start[alloc_diff .. alloc_size];
        _next_alloc_start = next_after_alloc;
        return mem;
    }

    size_t capacity() {
        return _num_bytes_reserved - (_next_alloc_start - _region_start);
    }

    void reserve(size_t n)
    in(n <= capacity) {
        n = round_to_page(n);
        const bytes_needed = min(max(_extra_bytes_per_alloc, n), capacity);

        version (Windows) {
            const status = VirtualAlloc(_region_end, bytes_needed, MEM_COMMIT, PAGE_READWRITE);
            if (!status)
                assert(0, "Failed to allocate memory.");
        }
        else
            static assert(false, "Platform not supported.");

        _region_end += bytes_needed;
    }

private:
    void* _region_start, _region_end, _next_alloc_start;

    const size_t _num_bytes_reserved;
    const size_t _extra_bytes_per_alloc;

    // The number of additional pages allocated whenever more pages are needed.
    enum extra_pages_per_alloc = 1000;

    size_t alloc_size(size_t size, size_t alignment) {
        const remainder = size % alignment;
        return size + (remainder ? size + alignment - remainder : remainder);
    }
}

/**
 Allocates memory in fixed-sized chunks. Freed chunks are
 tracked and reused for new allocations. It does not
 allocate memory for the chunks directly, instead passing
 allocation requests on to an instance of VirtualMemory.
 */
struct MemoryPool {
public:
    this(VirtualMemory* allocator, size_t chunk_size) {
        _allocator = allocator;
        _chunk_size = chunk_size;
    }

    @disable this(this);

    size_t chunk_size() {
        return _chunk_size;
    }

    void[] alloc() {
        if (_first_free_node is null)
            return _allocator.alloc(_chunk_size);

        auto mem = (cast(void*) _first_free_node)[0 .. _chunk_size];
        _first_free_node = _first_free_node.next;
        mem[] = null;
        return mem;
    }

    void free(void[] object)
    in(object.length == chunk_size) {
        auto n = cast(Node*)&object[0];
        n.next = _first_free_node;
        _first_free_node = n;
    }

private:
    struct Node {
        Node* next;
    }

    VirtualMemory* _allocator;
    size_t _chunk_size;
    Node* _first_free_node;
}

/**
 A specialization of MemoryPool for value types, it allows
 memory management to be handled on the scale of objects
 instead of blocks of bits.
 */
struct ObjectPool(T) {
    this(VirtualMemory* mem) {
        _chunks = MemoryPool(mem, T.sizeof);
    }

    @disable this(this);

    T* alloc(Args...)(Args args) {
        auto object = cast(T*) _chunks.alloc().ptr;
        *object = T(args); // could be a std.algorithm.moveEmplace?
        return object;
    }

    void free(T* t) {
        _chunks.free(t[0 .. 1]); // automatic conversion from T[1] to void[T.sizeof]
    }

    private MemoryPool _chunks;
}

@("Virtual Allocator and Object Pool")
unittest {
    auto vm = VirtualMemory(4.kib);

    {
        const m1 = vm.alloc(10);
        assert(m1.ptr !is null);
        assert(m1.length == 10);
    }

    {
        struct T {
            bool a;
            size_t b;
        }

        auto ts = ObjectPool!T(&vm);

        auto t1 = ts.alloc();
        assert(t1 !is null);
        assert(ts._chunks._first_free_node is null);

        auto t2 = ts.alloc();
        assert(t2 !is null);
        assert(ts._chunks._first_free_node is null);

        ts.free(t1);
        assert(ts._chunks._first_free_node is cast(void*) t1);

        auto t3 = ts.alloc();
        assert(t3 !is null);
        assert(ts._chunks._first_free_node is null);
        assert(t3 == t1);

        ts.free(t2);
        ts.free(t3);

        assert(ts.alloc() == t3);
        assert(ts.alloc() == t2);
    }
}

struct ArrayAllocator(T) {
public:
    this(VirtualMemory* memory, in size_t[] size_classes) {
        _size_classes = size_classes;
        _memory = memory;

        _chunks = cast(MemoryPool[]) _memory.alloc(MemoryPool.sizeof * size_classes.length);
        foreach (size_class_index, ref pool; _chunks)
            pool = MemoryPool(_memory, size_of(size_class_index));
    }

    T[] alloc_size_class(int class_index) {
        import std.conv : emplace;

        auto memory = cast(Header*) _chunks[class_index].alloc().ptr;
        auto list = memory.emplace!Header(class_index);
        return list.objects.ptr[0 .. _size_classes[class_index]];
    }

    Appender!T get_appender() {
        return Appender!T(&this);
    }

    void expand(ref T[] array) {
        auto header = header_of(array);
        auto new_array = alloc_size_class(header.class_index + 1);
        new_array[0 .. array.length] = array;
        free(array);
        array = new_array;
    }

    void free(T[] array) {
        auto header = header_of(array);
        _chunks[header.class_index].free((cast(void*) header)[0 .. size_of(header.class_index)]);
    }

private:
    struct Header {
        int class_index;
        ubyte[4] padding;
        T[0] objects;
    }

    size_t size_of(size_t class_index) {
        return Header.sizeof + (void*).sizeof * _size_classes[class_index];
    }

    Header* header_of(T[] array) {
        return (cast(Header*) array.ptr) - 1;
    }

    VirtualMemory* _memory;
    MemoryPool[] _chunks;

    const size_t[] _size_classes;
}

struct Appender(T) {
    this(ArrayAllocator!T* allocator) {
        _memory = allocator;
        _array = allocator.alloc_size_class(0);
    }

    @disable this(this);

    T[] get() {
        return _array[0 .. _count];
    }

    void abort() {
        _memory.free(_array);
        _array = [];
    }

    void opOpAssign(string op = "~")(T new_element) {
        if (_count == _array.length)
            _memory.expand(_array);

        _array[_count] = new_element;
        _count++;
    }

private:
    T[] _array;
    size_t _count;
    ArrayAllocator!T* _memory;
}

@("Array Allocator") unittest {
    auto memory = VirtualMemory(1024);
    auto allocator = ArrayAllocator!uint(&memory, [3, 5, 8]);

    auto v = 100u;

    auto c = allocator.alloc_size_class(0);
    assert(c.length == 3);
    c[1] = v;
    allocator.expand(c);
    assert(c.length == 5);
    assert(c[1] is v);
    allocator.expand(c);
    assert(c.length == 8);
    assert(c[1] is v);
    allocator.free(c);
    const d = allocator.alloc_size_class(2);
    assert(c.ptr == d.ptr);

    auto appender = allocator.get_appender();
    foreach (i; 0 .. 6)
        appender ~= v;

    assert(appender.get().length == 6);
    assert(appender.array.length == 8);
}
