module arc.data.type;

import arc.data.hash : Hash;
import arc.data.symbol : SymbolId;

struct TypeId {
    ushort value;
}

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

final class ArcTypeAllocator {
    import arc.memory: VirtualArray;

    enum max_types = (cast(size_t) TypeId.value.max) + 1;

public:
    this() {
        _id_type_map = VirtualArray!ArcType(max_types);
    }

    ArcType* type_of(TypeId id) {
        return &_id_type_map[id.value];
    }

    TypeId save_type(ArcType type) {
        const id = TypeId(cast(ushort) _id_type_map.length);

        _id_type_map ~= type;

        return id;
    }

private:
    VirtualArray!ArcType _id_type_map;
}
