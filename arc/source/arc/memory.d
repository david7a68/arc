module arc.memory;

import core.memory: pageSize;

size_t kib(size_t n) { return n * 1024; }
size_t mib(size_t n) { return n * (1024 * 2); }
size_t gib(size_t n) { return n * (1024 ^ 3); }

size_t round_to_nearest_page_size(size_t n) { return n + (pageSize - n % pageSize); }

/**
 * An object pool is an allocator for fixed-sized objects such as AST nodes.
 * Allocated objects are returned as an index, as though they were allocated
 * from an array with a simple `top` index. Doing has two benefits: smaller
 * 'pointer' sizes, and unambiguous ownership. The second benefit requires a
 * little more explanation. Essentially, all memory allocated from this region
 * is _owned_ by this region, and will be freed upon its destruction.
 *
 * There is also a side benefit, in that when processing allocated elements,
 * their positions in memory are constrained to a very specific area, and they
 * are allocated sequentially. This improves data locality, though how much
 * depends on the contents of the elements, as well as the mechanism and order
 * they are retrieved.
 */
struct IndexedRegion(T, Index) {
    import core.memory: pageSize;

private:
    void[] _pool;

    /// The number of bytes that have been prepared for use
    size_t _bytes_used;

    /// The number of allocated slots.
    Index _num_allocated;

public:
    /**
     * Reserve enough virtual memory to hold max_objects.
     */
    this(size_t max_objects) in (max_objects <= Index.max) {
        version (Windows) {
            import core.sys.windows.windows: VirtualAlloc, MEM_RESERVE, PAGE_NOACCESS;

            const n_bytes = max_objects * T.sizeof;
            const reserve_size = n_bytes + (pageSize - n_bytes % pageSize);
            auto addr = VirtualAlloc(null, reserve_size, MEM_RESERVE, PAGE_NOACCESS);
            _pool = addr[0 .. reserve_size];
        }
        else
            static assert(false, "Platform not supported");
    }

    @disable this(this);

    ~this() {
        version (Windows) {
            import core.sys.windows.windows: VirtualFree, MEM_RELEASE;

            const status = VirtualFree(_pool.ptr, 0, MEM_RELEASE);
            assert(status, "Failed to free memory for AST nodes!");
        }
        else
            static assert(false, "Platform not supported for IndexedRegion.");
    }

    /**
     * Checks if the allocator is at capacity. If this is the case, no more
     * allocations can be made.
     */
    bool is_at_capacity() {
        return _num_allocated * T.sizeof > _pool.length;
    }

    /// The number of allocated nodes
    Index num_allocated() { return _num_allocated; }

    void reserve(Index n_elements_to_reserve) {
        const bytes_to_reserve = n_elements_to_reserve * T.sizeof;
        assert(bytes_to_reserve + _num_allocated <= _pool.length);

        version (Windows) {
            import core.sys.windows.windows: VirtualAlloc, MEM_COMMIT, PAGE_READWRITE;

            const status = VirtualAlloc(_pool.ptr + _bytes_used, bytes_to_reserve, MEM_COMMIT, PAGE_READWRITE);
            assert(status, "Failed to reserve memory for AST nodes!");
        }
        else static assert(false, "Platform not supported for IndexedRegion.");
    }

    /**
     * Allocates the next slot in the sequence.
     */
    Index alloc() in (!is_at_capacity()) {
        // prepare memory for use here. We assume that alloc() is going to
        // happen a lot less frequently than get(), so put most of the work in
        // the less-called function.
        const first_byte_offset = _num_allocated * T.sizeof;
        if (first_byte_offset + T.sizeof >= _bytes_used) {
            auto next_page = _pool.ptr + _bytes_used;
            _bytes_used += pageSize;

            version (Windows) {
                import core.sys.windows.windows: VirtualAlloc, MEM_COMMIT, PAGE_READWRITE;

                const status = VirtualAlloc(next_page, T.sizeof, MEM_COMMIT, PAGE_READWRITE);
                assert(status, "Failed to allocate memory for AST nodes!");
            }
            else static assert(false, "Platform not supported for IndexedRegion.");
        }

        auto index = _num_allocated;
        _num_allocated++;

        // auto-initialize the memory
        *get(index) = T.init;
        return index;
    }

    /**
     * Returns the memory of the slot associated with the index
     */
    T* get(Index index) in (index < _num_allocated) {
        const first_byte_offset = index * T.sizeof;
        return cast(T*) (_pool.ptr + first_byte_offset);
    }

    /**
     * Retrieves a slice over all the allocated objects in this region.
     */
    T[] get_all() {
        return (cast(T*) _pool.ptr)[0 .. _num_allocated];
    }
}

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

    ~this() {
        version (Windows) {
            import core.sys.windows.windows: VirtualFree, MEM_RELEASE;

            const status = VirtualFree(base, 0, MEM_RELEASE);
            assert(status, "Failed to free memory.");
        }
        else
            static assert(false, "Platform not supported for IndexedRegion.");
    }

    void[] allocate(size_t n) {
        const next_after_alloc = next_alloc + n;
        const max_range_end = base + num_reserved_bytes;

        if (next_after_alloc > max_range_end) return null;

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

    T* allocate() {
        num_nodes++;

        if (head) {
            scope (exit) head = head.next;
            return &head.object;
        }

        return cast(T*) &(allocator.allocate(T.sizeof))[0];
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
