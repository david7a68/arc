module arc.data.symbol;

import arc.data.ast: AstNodeId;
import arc.data.hash : Hash;
import arc.data.type : TypeId;
import arc.indexed_allocator;

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
    AstNodeId declaration_id;
    TypeId type_id;
}

alias SymbolId = AllocIndex!(uint, Symbol);

final class GlobalSymbolTable : SimpleIndexedAllocator!(uint, Symbol) {
    const SymbolId none;
    const SymbolId unresolved;

    this() {
        super();
        none = save(Symbol(Symbol.Kind.None));
        unresolved = save(Symbol(Symbol.Kind.Unresolved));
    }

    SymbolId make_symbol(Symbol.Kind kind, Hash name) {
        return save(Symbol(kind, name));
    }
}
