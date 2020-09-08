module arc.data.scopes;

import arc.data.symbol;
import arc.data.hash;
import arc.indexed_allocator;

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

alias ScopeId = AllocIndex!(uint, Scope);

final class ScopeAllocator : SimpleIndexedAllocator!(uint, Scope) {

    // 96 gib
    enum max_scopes = (cast(size_t) ScopeId.value.max) + 1;

public:
    this(GlobalSymbolTable gst) {
        super();
        _symbols = gst;
        _null_scope_id = save(Scope());
    }

    ScopeId null_scope_id() {
        return _null_scope_id;
    }

    Scope* scope_of(ScopeId id) {
        return &this[id];
    }

    ScopeId make_scope(ScopeId parent) {
        return save(Scope(scope_of(parent), _symbols));
    }

private:
    GlobalSymbolTable _symbols;
    ScopeId _null_scope_id;
}
