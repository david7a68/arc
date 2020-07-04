module arc.data.type;

import arc.data.hash : Hash;
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
        ListMemberType[] members;
        FunctionType function_type;
    }
}

struct ListMemberType {
    Hash name;
    ArcType* type;
    size_t position;
}

struct FunctionType {
    ListMemberType[] parameters;
    ArcType* return_type;
}
