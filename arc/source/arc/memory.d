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

    T* alloc(T)()
    in (alloc_size(T.sizeof, standard_alignment) <= capacity) {
        return cast(T*) alloc(T.sizeof).ptr;
    }

    /**
     Frees all memory allocaated after this pointer a la a bump allocator.
     */
    void free_to_ptr(void* m) {
        assert(_region_start <= m && m < _next_alloc_start);
        assert((m - _region_start) % standard_alignment == 0);

        _next_alloc_start = m;
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
        assert(_allocator);
        if (_first_free_node is null)
            return _allocator.alloc(_chunk_size);

        auto mem = (cast(void*) _first_free_node)[0 .. _chunk_size];
        _first_free_node = _first_free_node.next;
        mem[] = null;
        return mem;
    }

    void free(void[] object)
    in(object.length == chunk_size) {
        assert(_allocator);
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
    static if (is(T == class)) {
        enum object_size = __traits(classInstanceSize, T);
        alias PtrType = T;
    }
    else {
        enum object_size = T.sizeof;
        alias PtrType = T*;
    }

    this(VirtualMemory* mem) {
        _chunks = MemoryPool(mem, object_size);
    }

    @disable this(this);

    PtrType alloc(Args...)(Args args) {
        import std.conv : emplace;

        auto object = cast(PtrType) _chunks.alloc().ptr;
        emplace(object, args);
        return object;
    }

    void free(PtrType t) {
        _chunks.free((cast(void*) t)[0 .. object_size]);
    }

    private MemoryPool _chunks;
}

/**
 An array pool provides a mechanism to allocate arrays of varying sizes from a
 virtual memory region, such that the array size does not need to be known
 beforehand.

 As arrays are promoted in size, the old array is preserved in a pool for future
 array allocations to use.

 In keeping with D's arrays, arrays of class types only contains references to
 memory where the actual objects reside and not the object's memory itself.
 Managing memory for classes is thus best done in conjunction with an object
 pool.
 */
struct ArrayPool(T) {
    static immutable size_t[] default_size_classes = [
        2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16_384,
        32_768, 65_536, 131_072, 262_144
    ];

public:
    this(VirtualMemory* memory, in size_t[] size_classes = default_size_classes)
    in(memory !is null) {
        _size_classes = size_classes;

        _chunks = cast(MemoryPool[]) memory.alloc(MemoryPool.sizeof * size_classes.length);
        foreach (size_class_index, ref pool; _chunks)
            pool = MemoryPool(memory, size_of(size_class_index));
    }

    T[] alloc(size_t length) {
        if (length == 0) return [];

        auto size_class = 0;
        for (; _size_classes[size_class] < length; size_class++) {
        }

        return alloc_size_class(size_class)[0 .. length];
    }

    T[] alloc_size_class(int class_index) {
        import std.conv : emplace;

        assert(_chunks);
        auto memory = cast(Header*) _chunks[class_index].alloc().ptr;
        auto list = memory.emplace!Header(class_index);
        return list.objects.ptr[0 .. _size_classes[class_index]];
    }

    T[] alloc_one_size_larger(T[] reference) {
        return alloc_size_class(header_of(reference).class_index + 1);
    }

    Appender!T get_appender() {
        assert(_chunks);
        return Appender!T(&this);
    }

    void expand(ref T[] array) {
        assert(_chunks);
        auto header = header_of(array);
        auto new_array = alloc_size_class(header.class_index + 1);
        new_array[0 .. array.length] = array;
        free(array);
        array = new_array;
    }

    void free(T[] array) {
        assert(_chunks);
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
        return Header.sizeof + T.sizeof * _size_classes[class_index];
    }

    Header* header_of(T[] array) {
        return (cast(Header*) array.ptr) - 1;
    }

    MemoryPool[] _chunks;

    const size_t[] _size_classes;
}

struct Appender(T) {
    this(ArrayPool!T* allocator)
    in(allocator !is null) {
        _memory = allocator;
        _array = allocator.alloc_size_class(0);
    }

    @disable this(this);

    T[] get() {
        return _array[0 .. _count];
    }

    void abort() {
        assert(_memory);
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
    ArrayPool!T* _memory;
}

struct TreeAllocator(T) {
    ArrayPool!(T*) arrays;
    ObjectPool!T objects;

    this(VirtualMemory* memory, in size_t[] size_classes = ArrayPool!(T*).default_size_classes) {
        arrays = ArrayPool!(T*)(memory, size_classes);
        objects = ObjectPool!T(memory);
    }
}
