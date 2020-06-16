module arc.data.symbol;

import arc.data.hash : Key;

struct Symbol {
    import arc.util: case_of;

    enum Kind : ubyte {
        None,
        Unknown,
        Unresolved,
        Char,
        Integer,
        String,
        List,
        Function,
        Import,
        ExprResult
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
