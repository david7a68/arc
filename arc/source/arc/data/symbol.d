module arc.data.symbol;

import arc.data.hash : Key;

struct Symbol {
    import arc.util: case_of;

    /**
     Different kinds of symbols can be used in different syntactical constructs
     such as binary operations, indexing, calling, etc.

     These serve as a kind of rough type system, which are further refined
     during type checking.
     */
    enum Kind : ubyte {
        None,
        Unknown,
        Unresolved,
        List,
        Function,
        FunctionParam,
        Import,
        Variable,
        Constant,
    }

public:
    Kind kind;
    private ubyte[7] padding;

    Key name;
    void* type;

    this(Kind kind, Key name) {
        this.kind = kind;
        this.name = name;
    }

    static unresolved() {
        static immutable _unresolved = Symbol(Kind.Unresolved, 0);
        return cast(Symbol*)&_unresolved;
    }

    bool is_marker() const {
        return kind == Kind.Unresolved;
    }
}
