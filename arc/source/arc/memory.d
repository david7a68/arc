module arc.memory;

import core.memory: pageSize;

size_t kib(size_t n) { return n * 1024; }
size_t mib(size_t n) { return n * (1024 * 2); }
size_t gib(size_t n) { return n * (1024 ^ 3); }

size_t round_to_nearest_page_size(size_t n) { return n + (pageSize - n % pageSize); }

/**
 Allocates a large span of virtual memory from the OS. The allocator works as a
 simple bump allocator, with no deallocation ability.
 */
struct VirtualMemory {
    import std.algorithm: max;

private:
    // The start of the virtual address range.
    void* base;
    // Points to the first byte of the next allocation.
    void* next_alloc;
    // One byte past the end of the writable address range.
    void* top;

    // The number of bytes reserved. This is a multiple of pageSize
    size_t num_reserved_bytes;

    // The number of additional pages allocated whenever more pages are needed.
    enum extra_pages_per_alloc = 1000;

    size_t extra_bytes_per_alloc;

public:
    this(size_t size_bytes) {
        extra_bytes_per_alloc = extra_pages_per_alloc * pageSize;

        // round to nearest page size
        num_reserved_bytes = max(round_to_nearest_page_size(size_bytes), extra_bytes_per_alloc);

        version (Windows) {
            import core.sys.windows.windows: VirtualAlloc, MEM_RESERVE, MEM_COMMIT, PAGE_NOACCESS, PAGE_READWRITE;

            base = VirtualAlloc(null, num_reserved_bytes, MEM_RESERVE, PAGE_NOACCESS);
            if (!base) assert(0, "Failed to allocate memory.");

            top = next_alloc = base;
        }
        else
            static assert(false, "Platform not supported.");
    }

    @disable this(this);

    ~this() {
        version (Windows) {
            import core.sys.windows.windows: VirtualFree, MEM_RELEASE;

            const status = VirtualFree(base, 0, MEM_RELEASE);
            assert(status, "Failed to free memory.");
        }
        else
            static assert(false, "Platform not supported for IndexedRegion.");
    }

    void[] alloc(size_t n) in (capacity >= n) {
        const next_after_alloc = next_alloc + n;

        if (next_after_alloc > top)
            reserve(next_after_alloc - top);

        auto mem = next_alloc[0 .. n];
        next_alloc += n;
        return mem;
    }

    size_t capacity() { return num_reserved_bytes - (next_alloc - base); }

    void reserve(size_t n) in (capacity >= n) {
        n = round_to_nearest_page_size(n);
        const bytes_needed = max(extra_bytes_per_alloc, n);

        version (Windows) {
            import core.sys.windows.windows: VirtualAlloc, MEM_COMMIT, PAGE_READWRITE;

            const status = VirtualAlloc(top, bytes_needed, MEM_COMMIT, PAGE_READWRITE);
            if (!status) assert(0, "Failed to allocate memory.");
        }
        else
            static assert(false, "Platform not supported.");

        top += bytes_needed;
    }
}

/**
 * An memory pool is an efficient way to allocate and deallocate fixed-sized
 * units of memory. Memory consumption grows only when the total number of
 * active allocations increases.
 */
struct MemoryPool {
private:
    struct Node { Node* next; }

    VirtualMemory* _allocator;
    size_t _object_size;
    Node* _head;

public:
    this(VirtualMemory* allocator, size_t object_size) {
        _allocator = allocator;
        _object_size = object_size;
    }

    @disable this(this);

    size_t object_size() { return _object_size; }

    void[] alloc() {
        if (_head is null) return _allocator.alloc(_object_size);

        auto mem = (cast(void*) _head)[0 .. _object_size];
        _head = _head.next;
        mem[] = null;
        return mem;
    }

    void free(void[] object) in (object.length == object_size) {
        auto n = cast(Node*) &object[0];
        n.next = _head;
        _head = n;
    }
}

/**
 * An object pool is an efficient way to allocate and deallocate lots of 
 * small objects. Memory consumption grows only when the total number of
 * active objects increases.
 */
struct ObjectPool(T) {
    private MemoryPool _pool;

    this(VirtualMemory* mem) { _pool = MemoryPool(mem, T.sizeof); }

    @disable this(this);

    T* alloc(Args...)(Args args) {
        auto object = cast(T*) _pool.alloc().ptr;

        static if (args.length) *object = T(args);
        else                    *object = T.init;

        return object;
    }

    void free(T* t) { _pool.free(t[0 .. 1]); }
}

@("Virtual Allocator and Object Pool")
unittest {
    auto vm = VirtualMemory(4.kib);

    {
        auto m1 = vm.alloc(10);
        assert(m1.ptr !is null);
        assert(m1.length == 10);
    }

    {
        struct T { bool a; size_t b; }
        auto ts = ObjectPool!T(&vm);

        auto t1 = ts.alloc();
        assert(t1 !is null);
        assert(ts._pool._head is null);

        auto t2 = ts.alloc();
        assert(t2 !is null);
        assert(ts._pool._head is null);

        ts.free(t1);
        assert(ts._pool._head is cast(void*) t1);

        auto t3 = ts.alloc();
        assert(t3 !is null);
        assert(ts._pool._head is null);
        assert(t3 == t1);

        ts.free(t2);
        ts.free(t3);

        assert(ts.alloc() == t3);
        assert(ts.alloc() == t2);
    }
}

struct TreeAllocator(T, size_t[] size_classes) {
    this(size_t max_objects) {
        _memory = VirtualMemory(max_objects * T.sizeof * 2);
        _objects = ObjectPool!T(&_memory);

        foreach (size_class_index, ref list_pool; _list_pools)
            list_pool = MemoryPool(&_memory, size_of(size_class_index));
    }

    T* alloc(Args...)(Args args) { return _objects.alloc(args); }

    void free(T* object) { _objects.free(object); }

    auto get_appender() { return Appender(&this); }

    T*[] alloc_array(int size_class_index) {
        import std.conv: emplace;

        auto memory = cast(ListHeader*) _list_pools[size_class_index].alloc().ptr;
        auto list = memory.emplace!ListHeader(size_class_index);
        return list.objects.ptr[0 .. size_classes[size_class_index]];
    }

    void expand_array(ref T*[] array) {
        import core.stdc.string: memcpy;

        auto header = header_of(array);

        auto new_array = alloc_array(header.size_class_index + 1);
        memcpy(header.objects.ptr, new_array.ptr, size_classes[header.size_class_index] * (T*).sizeof);
        free_array(array);

        array = new_array;
    }

    void free_array(T*[] array) {
        auto header = header_of(array);
        _list_pools[header.size_class_index].free((cast(void*) header)[0 .. size_of(header.size_class_index)]);
    }

private:
    struct ListHeader {
        int size_class_index;
        ubyte[4] padding;
        T*[0] objects;
    }

    struct Appender {
        TreeAllocator* memory;
        T*[] array;
        size_t count;

        this(TreeAllocator* allocator) {
            memory = allocator;
            array = allocator.alloc_array(0);
        }

        @disable this(this);

        T*[] get() { return array[0 .. count]; }

        void abort() { memory.free_array(array); }

        void opOpAssign(string op = "~")(T* new_element) {
            if (count == array.length) memory.expand_array(array);

            array[count] = new_element;
            count++;
        }
    }

    size_t size_of(size_t size_class_index) {
        return ListHeader.sizeof + (void*).sizeof * size_classes[size_class_index];
    }

    ListHeader* header_of(T*[] array) {
        return (cast(ListHeader*) array.ptr) - 1;
    }

    VirtualMemory                   _memory;
    ObjectPool!T                    _objects;
    MemoryPool[size_classes.length] _list_pools;
}
