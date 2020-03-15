module arc.stringtable;

import arc.hash: Key;

final class StringTable {
    bool opBinary(string op = "in")(String s) {
        return (s.key in table) !is null;
    }

    Key insert(const(char)[] text) {
        auto key = StringTable.digest(text);
        table[key] = text;
        return key;
    }

    Key insert(const(char)[] text, Key key) {
        table[key] = text;
        return key;
    }

    const(char)[] lookup(Key key) {
        return table.get(key, []);
    }

    static Key digest(const(char)[] text) {
        import arc.hash: digest;
        
        return text.digest();
    }

private:
    alias Text = const(char)[];
    Text[Key] table;
}
