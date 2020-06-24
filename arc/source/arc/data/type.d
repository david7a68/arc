module arc.data.type;

import arc.data.hash : Key;
import arc.data.symbol : Symbol;

struct ArcType {
    enum Kind {
        None,
        Inferred,
        Void,
        List,
        Function,
        Integer,
        Character,
        String,
        Pointer,
    }

public:
    Kind kind;

    size_t id;
    size_t alignment;
    size_t num_bytes;

    void[] default_value;

    union {
        ArcType* base_type;
        ListMember[] members;
        FunctionType function_type;
    }

    static inferred() {
        static immutable _auto = ArcType(Kind.Inferred, 0, 0, 0);
        return cast(ArcType*)&_auto;
    }

    static builtin_uint() {
        static immutable _uint = ArcType(Kind.Integer, 0, 8, 8);
        return cast(ArcType*)&_uint;
    }

    static builtin_char() {
        static immutable _char = ArcType(Kind.Character, 0, 1, 1);
        return cast(ArcType*)&_char;
    }
}

struct ListMember {
    Key name;
    ArcType* type;
    size_t position;
}

struct FunctionType {
    ListMember[] parameters;
    ArcType* return_type;
}

struct ArcTypeAllocator {
    import arc.memory : VirtualMemory, ObjectPool, ArrayPool;

    ObjectPool!ArcType types;
    ArrayPool!ListMember member_arrays;

    this(VirtualMemory* vm) {
        types = ObjectPool!ArcType(vm);
        member_arrays = ArrayPool!ListMember(vm);
    }

    ArcType* make_type(ArcType.Kind kind) {
        return types.alloc(kind);
    }
}
