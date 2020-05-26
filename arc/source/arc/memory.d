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

    void[] allocate(size_t n) in (capacity >= n) {
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
 * An object pool is an efficient way to allocate and deallocate fixed-sized
 * units of memory. Memory consumption grows only when the total number of
 * active objects increases.
 */
struct ObjectPool(T) if (!is(T == class) && !is(T == interface)) {
private:
    union Node {
        Node* next;
        T object;
    }

    VirtualAllocator* allocator;
   
    Node* head;
    size_t num_nodes;

public:
    this(VirtualAllocator* allocator) {
        this.allocator = allocator;
    }

    @disable this(this);

    T* allocate() {
        num_nodes++;

        if (!head)
            return cast(T*) &(allocator.allocate(T.sizeof))[0];

        scope (exit) head = head.next;
        return &head.object;
    }

    void deallocate(T* object) {
        auto n = cast(Node*) object;
        n.next = head;
        head = n;
        num_nodes--;
    }
}

@("Virtual Allocator and Object Pool")
unittest {
    auto vm = VirtualAllocator(4.kib);

    {
        auto m1 = vm.allocate(10);
        assert(m1.ptr !is null);
        assert(m1.length == 10);
    }

    {
        struct T { bool a; size_t b; }
        auto ts = ObjectPool!T(&vm);

        auto t1 = ts.allocate();
        assert(t1 !is null);
        assert(ts.num_nodes == 1);
        assert(ts.head is null);

        auto t2 = ts.allocate();
        assert(t2 !is null);
        assert(ts.num_nodes == 2);
        assert(ts.head is null);

        ts.deallocate(t1);
        assert(ts.num_nodes == 1);
        assert(ts.head !is null);

        auto t3 = ts.allocate();
        assert(t3 !is null);
        assert(ts.num_nodes == 2);
        assert(ts.head is null);
        assert(t3 == t1);

        ts.deallocate(t2);
        ts.deallocate(t3);

        assert(ts.num_nodes == 0);
        assert(ts.allocate() == t3);
        assert(ts.allocate() == t2);
    }
}
