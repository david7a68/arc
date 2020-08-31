module arc.data.symbol;

import arc.data.hash : Hash;

struct SymbolId {
    uint value;
}

struct Symbol {
    enum Kind : ubyte {
        None,
        Unknown,
        Unresolved,
        FunctionParam,
        Import,
        Variable,
        Constant,
        TypeName
    }

    Kind kind;
    Hash name;
}

struct GlobalSymbolTable {
    import arc.memory : VirtualArray;

    enum max_symbols = SymbolId.value.max + 1;

public:
    this(size_t initial_size) {
        _symbols = VirtualArray!Symbol(max_symbols);
        _symbols.reserve(initial_size);

        _none = make_symbol(Symbol.Kind.None, Hash());
        _unresolved = make_symbol(Symbol.Kind.Unresolved, Hash());
    }

    SymbolId unresolved() {
        return _unresolved;
    }

    SymbolId none() {
        return _none;
    }

    SymbolId make_symbol(Symbol.Kind kind, Hash name) {
        const id = SymbolId(cast(uint) _symbols.length);
        _symbols ~= Symbol(kind, name);
        return id;
    }

    Symbol* symbol_of(SymbolId id) {
        return &_symbols[id.value];
    }

private:
    VirtualArray!Symbol _symbols;
    SymbolId _none, _unresolved;
}
