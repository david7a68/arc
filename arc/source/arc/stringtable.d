module arc.stringtable;

import arc.hash;

struct StringTable {
    bool opBinary(string op = "in")(String s) {
        return (s.key in table) !is null;
    }

    Key insert(const(char)[] text) {
        auto key = text.digest();
        table[key] = text;
        return key;
    }

private:
    alias Text = const(char)[];
    Text[Key] table;
}
