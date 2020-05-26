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
struct VirtualAllocator {
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

    VirtualAllocator* _allocator;
    size_t _object_size;
    Node* _head;

public:
    this(VirtualAllocator* allocator, size_t object_size) {
        _allocator = allocator;
        _object_size = object_size;
    }

    @disable this(this);

    size_t object_size() { return _object_size; }

    void[] alloc() {
        if (!_head) return _allocator.alloc(_object_size);

        scope (exit) _head = _head.next;
        return (cast(void*) _head)[0 .. _object_size];
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
    private MemoryPool pool;

    this(VirtualAllocator* mem) { pool = MemoryPool(mem, T.sizeof); }

    @disable this(this);

    T* alloc(Args...)(Args args) {
        auto object = cast(T*) pool.alloc().ptr;

        static if (args.length) *object = T(args);
        else                    *object = T.init;
        
        return object;
    }

    void free(T* t) { pool.free(t[0 .. 1]); }
}

@("Virtual Allocator and Object Pool")
unittest {
    auto vm = VirtualAllocator(4.kib);

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
        assert(ts.pool._head is null);

        auto t2 = ts.alloc();
        assert(t2 !is null);
        assert(ts.pool._head is null);

        ts.free(t1);
        assert(ts.pool._head !is null);

        auto t3 = ts.alloc();
        assert(t3 !is null);
        assert(ts.pool._head is null);
        assert(t3 == t1);

        ts.free(t2);
        ts.free(t3);

        assert(ts.alloc() == t3);
        assert(ts.alloc() == t2);
    }
}
