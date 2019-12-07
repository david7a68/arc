module arc.output.symbol_printer;

import arc.semantic.symbol;

struct SymbolPrinter {
    import std.array: appender, Appender;
    import std.container.array: Array;

    import arc.stringtable: StringTable;

    enum IndentType: ubyte {
        None, Space, Bar
    }

    static immutable indent_str = ["NONE", "    ", " │  "];
    enum tbar = " ├─ ";
    enum lbar = " └─ ";

    Array!IndentType stack;
    Appender!(char[]) str;
    StringTable* strings;

    this(StringTable* strings) {
        str = appender!(char[]);
        this.strings = strings;
    }

    const(char)[] data() {
        return cast(const(char)[]) str.data;
    }

    void reset() {
        str.clear();
    }

    void print(Symbol* symbol) {
        switch (symbol.kind) with (Symbol.Kind) {
            case Unknown:
                str.put("Unknown\n");
                break;
            case Definition:
                str.put("Define \"");
                str.put(strings.lookup(symbol.key));
                str.put("\"\n");
                break;
            case Variable:
                str.put("Variable \"");
                str.put(strings.lookup(symbol.key));
                str.put("\"\n");
                break;
            case Lookup:
                str.put("Lookup ");
                if (symbol.referenced_symbol is null) {
                    str.put("unresolved \"");
                    str.put(strings.lookup(symbol.key));
                    str.put("\"\n");
                }
                else {
                    str.put("\"");
                    str.put(strings.lookup(symbol.key));
                    str.put("\"\n");
                }
                break;
            case Function:
                str.put("Function\n");
                print_children(symbol);
                break;
            case Scope:
                str.put("Scope\n");
                print_children(symbol);
                break;
            default:
                assert(false);
        }
    }

    void print_children(Symbol* parent) {
        const num_symbols = parent.symbols.length;
        foreach (i, nested_symbol; parent.symbols) {
            foreach (type; stack[])
                str.put(indent_str[type]);
            
            if (i + 1 != num_symbols) {
                str.put(tbar);
                stack.insertBack(IndentType.Bar);
            }
            else {
                str.put(lbar);
                stack.insertBack(IndentType.Space);
            }

            print(nested_symbol);
            stack.removeBack();
        }
    }
}