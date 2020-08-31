module arc.data.stringtable;

struct StringTable {
    import arc.data.hash: Hash, hash_of;

public:
    Hash intern(const char[] str) {
        auto key = hash_of(str);
        _table[key] = str.idup;
        return key;
    }

    string string_of(Hash hash) {
        return _table.get(hash, "");
    }

private:
    string[Hash] _table;
}
