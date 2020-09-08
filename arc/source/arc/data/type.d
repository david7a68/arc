module arc.data.type;

import arc.data.hash : Hash;
import arc.data.symbol : SymbolId;
import arc.indexed_allocator;

struct ArcType {
    enum Kind {
        Unknown,
        TypeError,
        Type,
        Integer,
        ConstInteger,
        Char,
    }

public:
    Kind kind;

    SymbolId declaration;

    size_t size;
    size_t alignment;
}


alias TypeId = AllocIndex!(ushort, ArcType);

alias ArcTypeAllocator = SimpleIndexedAllocator!(ushort, ArcType);
