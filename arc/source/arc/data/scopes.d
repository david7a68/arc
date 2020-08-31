module arc.data.scopes;

import arc.data.symbol;
import arc.data.hash;

struct ScopeId {
    uint value;
}

struct Scope {
    Scope* parent() {
        return _parent;
    }

    void add(SymbolId id) {
        _symbols[id] = _all_symbols.symbol_of(id);
    }

    SymbolId lookup(Hash name) {
        return SymbolId(0);
    }

private:
    this(Scope* parent, GlobalSymbolTable* gst) {
        _parent = parent;
        _all_symbols = gst;
    }

    Scope* _parent;
    GlobalSymbolTable* _all_symbols;
    Symbol*[SymbolId] _symbols;
}

struct ScopeAllocator {
    import arc.memory : VirtualArray;

    // 96 gib
    enum max_scopes = (cast(size_t) ScopeId.value.max) + 1;

public:
    this(GlobalSymbolTable* gst) {
        _symbols = gst;
        
        _scopes = VirtualArray!Scope(max_scopes);

        _null_scope_id = ScopeId(0);
        _scopes ~= Scope();
    }

    ~this() {
        destroy(_scopes);
    }

    ScopeId null_scope_id() {
        return _null_scope_id;
    }

    Scope* scope_of(ScopeId id) {
        return &_scopes[id.value];
    }

    ScopeId make_scope(ScopeId parent) {
        const id = ScopeId(cast(uint) _scopes.length);

        _scopes ~= Scope(scope_of(parent), _symbols);
        
        return id;
    }

private:
    GlobalSymbolTable* _symbols;

    VirtualArray!Scope _scopes;

    ScopeId _null_scope_id;
}
