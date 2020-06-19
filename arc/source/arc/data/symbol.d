module arc.data.symbol;

import arc.data.hash : Key;

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

struct SymbolTable {
    import arc.memory : ArrayPool, ObjectPool, VirtualMemory;

public:
    this(VirtualMemory* memory) {
        _symbols = ObjectPool!Symbol(memory);
        _tables = ObjectPool!ScopedSymbolTable(memory);
        _table_entries = ArrayPool!(ScopedSymbolTable.Entry)(memory);

        // Push global scope
        _root = _current = _tables.alloc(_current, &_table_entries);
    }

    size_t num_scopes() {
        return _num_scopes;
    }

    size_t num_symbols() {
        return _num_symbols;
    }

    ScopedSymbolTable* push_scope() {
        _current = _tables.alloc(_current, &_table_entries);
        _num_scopes++;
        return _current;
    }

    Symbol* make_symbol(Args...)(Args args) {
        auto symbol = _symbols.alloc(args);
        _current.put(symbol.name, symbol);
        _num_symbols++;
        return symbol;
    }

    void pop_scope() {
        _current = _current.parent;
    }

private:
    size_t _num_scopes;
    size_t _num_symbols;

    VirtualMemory* _vm;
    ScopedSymbolTable* _root;
    ScopedSymbolTable* _current;

    ObjectPool!Symbol _symbols;
    ObjectPool!ScopedSymbolTable _tables;
    ArrayPool!(ScopedSymbolTable.Entry) _table_entries;
}

struct ScopedSymbolTable {
    import arc.memory : ArrayPool;

public:
    this(ScopedSymbolTable* parent, ArrayPool!Entry* arrays) {
        _parent = parent;
        _arrays = arrays;
        _entries = _arrays.alloc_size_class(0);
    }

    ScopedSymbolTable* parent() {
        return _parent;
    }

    void put(Key key, Symbol* value) {
        if (_count >= 0.6 * _entries.length)
            resize();

        _entries[find_slot_for(key, _entries)] = Entry(key, value);
        _count++;
    }

    Symbol* get(Key key)
    in(contains(key)) {
        return _entries[find_slot_for(key, _entries)].value;
    }

    bool contains(Key key) {
        return _entries[find_slot_for(key, _entries)].key == key;
    }

    version (unittest) Symbol*[] get_all_symbols() {
        import std.array : appender;

        auto ap = appender(new Symbol*[](_count));

        foreach (entry; _entries)
            if (entry != Entry())
                ap ~= entry.value;

        return ap[];
    }

private:
    struct Entry {
        // Duplicated from value so we don't need to look up value
        Key key;
        Symbol* value;
    }

    /// linear probing
    static size_t find_slot_for(Key key, Entry[] map) {
        auto index = key % map.length;

        while (map[index].key != key && map[index] != Entry()) {
            if (index + 1 < map.length)
                index++;
            else
                index = 0;
        }

        return index;
    }

    void resize() {
        auto new_array = _arrays.alloc_one_size_larger(_entries);

        foreach (i, entry; _entries) {
            if (entry != Entry())
                new_array[find_slot_for(entry.key, new_array)] = _entries[i];
        }

        _arrays.free(_entries);
        _entries = new_array;
    }

    ScopedSymbolTable* _parent;
    ArrayPool!Entry* _arrays;
    Entry[] _entries;
    size_t _count;
}
