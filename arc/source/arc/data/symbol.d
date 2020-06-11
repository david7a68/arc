module arc.data.symbol;

import arc.data.hash: Key;

/**
 A symbol is represents a single usage of a named entity.
 */
struct Symbol {
    enum Kind {
        None,
        Type,
        Value,
        Variable,
        Function,
        Import,
        ListMember,
        Namespace
    }

    Key name;
}
