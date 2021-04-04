module arc.indexed_allocator;

struct AllocIndex(IdType, ObjType) {
    enum max_indices = (cast(size_t) IdType.max) + 1;

    IdType value;
}

/**
 An indexed allocator is an allocator that assigns an index to each allocation,
 such that an object may be referenced by its pointer or by its index.
 */
interface IndexedAllocator(IdType, ObjType) {
    alias Index = AllocIndex!(IdType, ObjType);
    
    Index save(ObjType);
    Index save(ref ObjType);
    ref ObjType opIndex(Index);
}

class SimpleIndexedAllocator(I, T) : IndexedAllocator!(I, T) {
    import arc.memory: VirtualArray;

    alias Index = AllocIndex!(I, T);

public:
    this() {
        _array = VirtualArray!T(Index.max_indices);
    }

    ~this() {
        destroy(_array);
    }

    override Index save(T object) {
        return save(object);
    }

    override Index save(ref T object) {
        const id = Index(cast(I) _array.length);
        _array ~= object;
        return id;
    }

    override ref T opIndex(Index index) {
        return _array[index.value];
    }

private:
    VirtualArray!T _array;
}
