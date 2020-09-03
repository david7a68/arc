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
        _symbols[_all_symbols[id].name] = id;
    }

    SymbolId get_local_id(Hash hash) {
        return _symbols.get(hash, SymbolId());
    }

    Symbol* get_local(Hash hash) {
        return _all_symbols[get_local_id(hash)];
    }

private:
    this(Scope* parent, GlobalSymbolTable gst) {
        _parent = parent;
        _all_symbols = gst;
    }

    Scope* _parent;
    GlobalSymbolTable _all_symbols;
    SymbolId[Hash] _symbols;
}

final class ScopeAllocator {
    import arc.memory : VirtualArray;

    // 96 gib
    enum max_scopes = (cast(size_t) ScopeId.value.max) + 1;

public:
    this(GlobalSymbolTable gst) {
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
    GlobalSymbolTable _symbols;

    VirtualArray!Scope _scopes;

    ScopeId _null_scope_id;
}
