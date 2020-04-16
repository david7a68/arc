module arc.semantic.type;

struct ArcType {
    import arc.hash: Key;
    import arc.ast: AstNode;

    enum Kind {
        Unknown,
        Aggregate,
        Function,
        Integer,
        Char,
        Float,
        Pointer,
        Named,
    }

    alias Kind this;

    Kind kind;
    Key name;

    union {
        ArcType*[] members;
        FunctionType func_type;
        Integer integer;
        Float floating;
        ArcType* pointer_type;
        AstNode* unknown_type;
    }
}

struct FunctionType {
    ArcType*[] parameters;
    ArcType* result;
}

struct Integer {
    size_t size;
    size_t min;
    size_t max;

    static immutable constant = Integer(128, 0, 2 ^ 128 - 1);
}

struct Float {
    size_t size;
}
