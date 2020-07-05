module arc.data.symbol;

import arc.data.hash : Hash;
import arc.data.type : ArcType;

struct Symbol {
    import arc.util : case_of;

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
        FunctionParam,
        Import,
        Variable,
        Constant,
        Builtin
    }

public:
    Kind kind;
    private ubyte[7] padding;

    Hash name;
    ArcType* type;
    SymbolTable* declaring_scope;

    this(Kind kind, Hash name, SymbolTable* declaring_scope, ArcType* type = null) {
        this.kind = kind;
        this.name = name;
        this.declaring_scope = declaring_scope;
        this.type = type;
    }

    static unresolved() {
        static immutable _unresolved = Symbol(Kind.Unresolved, 0, null);
        return cast(Symbol*)&_unresolved;
    }

    bool is_marker() const {
        return kind == Kind.Unresolved;
    }
}

struct SymbolTable {
    import arc.data.structures: KeyMap;
    import arc.memory: VirtualMemory;

public:
    this(VirtualMemory* vm) {
        _vm = vm;
        _symbols = KeyMap!(Symbol*)(64);
    }

    this(VirtualMemory* vm, SymbolTable* parent) {
        this(vm);
        _parent = parent;
    }

    SymbolTable* parent() {
        return _parent;
    }

    Symbol* make_symbol(Symbol.Kind kind, Hash hash) {
        auto sym = _vm.alloc!Symbol();
        *sym = Symbol(kind, hash, &this);
        _symbols.insert(hash, sym);
        return sym;
    }

    Symbol* get(Hash hash) {
        return _symbols.get(hash);
    }

    bool contains(Hash hash) {
        return _symbols.contains(hash);
    }

    Symbol* lookup(Hash hash) {
        return _symbols.get(hash, _parent ? _parent.lookup(hash) : null);
    }

private:
    VirtualMemory* _vm;
    KeyMap!(Symbol*) _symbols;
    SymbolTable* _parent;
}

struct SymbolTreeBuilder {
    import arc.memory : VirtualMemory;

public:
    this(VirtualMemory* memory, SymbolTable* root) {
        _vm = memory;
        _root = _current = root;
    }

    SymbolTable* current_scope() {
        return _current;
    }

    SymbolTable* push_scope() {
        _current = _vm.alloc!SymbolTable();
        *_current = SymbolTable(_vm, _current);
        return _current;
    }

    Symbol* make_symbol(Symbol.Kind kind, Hash hash) {
        return _current.make_symbol(kind, hash);
    }

    void pop_scope() {
        _current = _current.parent;
    }

private:
    size_t _num_scopes;
    size_t _num_symbols;

    VirtualMemory* _vm;
    SymbolTable* _root;
    SymbolTable* _current;
}
