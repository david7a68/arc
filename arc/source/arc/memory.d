module arc.memory;

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
