module arc.data.stringtable;

struct StringTable {
    import arc.data.hash: Hash, hash_of;
    import arc.data.structures: KeyMap;
    import arc.memory: VirtualMemory, gib;

    enum storage_capacity = 128.gib;

public:
    this(size_t initial_size) {
        _storage = VirtualMemory(storage_capacity);
        _table = KeyMap!string(initial_size);
    }

    Hash intern(const char[] str) {
        auto mem = (cast(char*) _storage.alloc(str.length).ptr)[0 .. str.length];
        mem[] = str;
        auto key = hash_of(mem);
        _table.insert(key, cast(string) mem);
        return key;
    }

private:
    VirtualMemory _storage;
    KeyMap!(string) _table;
}