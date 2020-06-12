module arc.data.symbol;

import arc.data.hash : Key;

/**
 A symbol is represents a single usage of a named entity.
 */
struct Symbol {
    enum Kind {
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

    Kind kind;
    Key name;

    static unresolved() {
        return cast(Symbol*)&_unresolved;
    }

    bool is_marker() const {
        return kind == Kind.Unresolved;
    }
}

private const _unresolved = Symbol(Symbol.Kind.Unresolved, 0);
