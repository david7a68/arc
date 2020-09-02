module arc.memory;

import core.memory : pageSize;

// dfmt off
size_t kib(size_t n) { return n * 1024; }
size_t mib(size_t n) { return n * (1024 ^^ 2); }
size_t gib(size_t n) { return n * (1024 ^^ 3); }
// dfmt on

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
        if (!_num_bytes_reserved)
            return;

        version (Windows) {
            const status = VirtualFree(_region_start, 0, MEM_RELEASE);
            assert(status, "Failed to free memory.");
        }
        else
            static assert(false, "Platform not supported for IndexedRegion.");
    }

    // dfmt off
    size_t bytes_reserved() { return _num_bytes_reserved; }

    size_t bytes_allocated() { return _next_alloc_start - _region_start; }
    
    void* base_ptr() { return _region_start; }
    // dfmt on

    size_t alloc_size_for(size_t size, size_t alignment = standard_alignment) {
        const remainder = size % alignment;
        return size + (remainder ? alignment - remainder : remainder);
    }

    /**
     Allocates n bytes of memory.

     For simplicity, this function will always return memory aligned to the
     standard alignment, which is described at the top of the struct.
     */
    void[] alloc(size_t n, size_t alignment = standard_alignment)
    in (alloc_size_for(n) <= capacity) {
        const alloc_size = alloc_size_for(n, alignment);
        const alignment_offset = alloc_size - n; // This subtraction could be avoided by unpacking alloc_size
        auto next_after_alloc = _next_alloc_start + alloc_size;

        if (next_after_alloc > _region_end)
            reserve(next_after_alloc - _region_end);

        auto mem = _next_alloc_start[alignment_offset .. alloc_size];
        _next_alloc_start = next_after_alloc;
        return mem;
    }

    /**
     Allocates an instance of type T without initializing it. Any initialization
     must be done after this function.
     */
    PtrType!T alloc(T, Args...)(Args args)
    in (alloc_size_for(object_size!T) <= capacity) {
        return alloc(object_size!T).emplace_obj!T(args);
    }

    size_t capacity() {
        return _num_bytes_reserved - (_next_alloc_start - _region_start);
    }

    void reserve(size_t n)
    in (n <= capacity) {
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

    size_t _num_bytes_reserved;
    size_t _extra_bytes_per_alloc;

    // The number of additional pages allocated whenever more pages are needed.
    enum extra_pages_per_alloc = 10;
}

struct VirtualArray(T) {
    this(size_t max_length) {
        _vm = VirtualMemory(T.sizeof * max_length);
    }

    // dfmt off
    size_t bytes_allocated() { return _vm.bytes_allocated; }

    size_t bytes_reserved() { return _vm.bytes_reserved; }

    size_t length() { return _vm.bytes_allocated / T.sizeof; }
    
    size_t capacity() { return _vm.bytes_reserved / T.sizeof; }
    
    PtrType!T base_ptr() { return cast(PtrType!T) _vm.base_ptr; }
    // dfmt on

    ref T opIndex(size_t index)
    in (index < length) {
        return base_ptr[index];
    }

    ref VirtualArray!T opOpAssign(string op = "~")(T object)
    in (length < capacity) {
        *(cast(PtrType!T) _vm.alloc(T.sizeof, T.alignof)) = object;
        return this;
    }

    void reserve(size_t count) {
        _vm.reserve(count * T.sizeof);
    }

private:
    VirtualMemory _vm;
}

private:

size_t round_to_page(size_t n) {
    return n + (pageSize - n % pageSize);
}

PtrType!T emplace_obj(T, Args...)(void[] mem, Args args)
in (mem.length >= object_size!T) {
    import std.conv : emplace;

    return (cast(PtrType!T) mem.ptr).emplace(args);
}

template object_size(T) {
    static if (is(T == class))
        enum object_size = __traits(classInstanceSize, T);
    else
        enum object_size = T.sizeof;
}

template PtrType(T) {
    static if (is(T == class))
        alias PtrType = T;
    else
        alias PtrType = T*;
}
