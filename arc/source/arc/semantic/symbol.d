module arc.semantic.symbol;

/**
 * A symbol represents a single unit of semantic information. Symbols tend to
 * map somewhat closely with the abstract syntax tree, but with additional
 * information.
 */
struct Symbol {
    import arc.syntax.ast: AstNode;
    import arc.hash: Key;

    enum Kind {
        Unknown,
        Definition,
        Variable,
        Lookup,
        Function,
        Scope,
    }

    alias Kind this;

    /// The type of symbol this represents
    Kind kind;
    /// The position of this symbol in its scope.
    uint scope_position;
    /// The parent symbol
    Symbol* parent;
    /// The syntax tree that this symbol was derived from
    AstNode syntax;

    union {
        struct {
            /// The symbol that this one is referring to
            Symbol* referenced_symbol;
            /// The hashed key that refers to the textual name for this symbol.
            Key key;
        }
        /// Symbols that belong within this symbol's scope
        Symbol*[] symbols;
    }
}
