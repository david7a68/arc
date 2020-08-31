module arc.output.ast_printer;

import arc.data.ast;
import arc.data.stringtable;
import std.format: format;

const(char[]) print_ast(SyntaxTree ast, StringTable* strings) {
    auto printer = AstPrinter(ast, strings);
    printer.print();
    return printer.data;
}

/**
 * AstVisitor that generates a string representation of the syntax tree.
 *
 * Formats an expression as an expanding tree to look something like the following:
 *
 *  ```
 *  a := (b-(square 19), some_var)
 *  ```
 *
 *  becomes
 *
 *  ```
 *  Variable
 *   ├─ a
 *   ├─ Inferred
 *   └─ List
 *       ├─ ListMember
 *       │    ├─ Type: Inferred
 *       |    └─ Expr: -
 *       │       ├─ b
 *       │       └─ square
 *       │          └─ 19
 *       └─ ListMember
 *            ├─ Inferred
 *            └─ Name
 *  ```
 */
struct AstPrinter {
    import std.array : appender, Appender;
    import std.container.array : Array;

    enum IndentType : ubyte {
        None,
        Space,
        Bar
    }

    static immutable indent_str = ["NONE", "    ", " │  "];
    enum tbar = " ├─ ";
    enum lbar = " └─ ";

    Array!IndentType stack;
    Appender!(char[]) str;
    SyntaxTree source;
    StringTable* strings;

    this(SyntaxTree source, StringTable* strings) {
        str = appender!(char[]);
        this.source = source;
        this.strings = strings;
    }

    const(char)[] data() const {
        return str.data;
    }

    void reset() {
        str.clear();
    }

    void print() {
        foreach (node_id; source.statements)
            print(node_id);
    }

    void print(AstNodeId node, string prefix = "") {
        str.put(prefix);

        str.put(source.match!(const(char)[])(
            node,
            (Char* n) => format("%s", n.kind),
            (String* n) => format("%s", n.kind),
            (Integer* n) => format("%s (%s)", n.kind, n.value),
            (SymbolRef* n) => format("%s", n.kind),
            (AstNode* n) => format("%s", n.kind),
            (Char* n) => format("%s (%s)", n.kind, strings.string_of(n.value)),
            (String* n) => format("%s (%s)", n.kind, strings.string_of(n.value)),
            (Integer* n) => format("%s (%s)", n.kind, n.value),
            (SymbolRef* n) => format("%s (%s)", n.kind, strings.string_of(n.name_hash)),
            (AstNode* n) => format("%s", n.kind)
        ));

        source.match!void(
            node,
            (Function* n) {
                write_named_children(node, "Params: ", "Return Type: ", "Body: ");
            },
            (List* n) {
                str.put(format(" (%s)", source.children_of(node).length));
                write_children(node, true);
            },
            (UnOp *n) {
                write_named_children(node, "Operand: ");
            },
            (BinOp *n) {
                if (n.kind == AstNode.Kind.Call)
                    write_named_children(node, "Target: ", "Arguments: ");
                else if (n.kind == AstNode.Kind.Access || n.kind == AstNode.Kind.StaticAccess)
                    write_named_children(node, "Source: ", "Member: ");
                else
                    write_named_children(node, "Left: ", "Right: ");
            },
            (If* n) {
                write_named_children(node, "Condition: ", "Body: ", "Else: ");
            },
            (ListMember* n) {
                // str.put(format(" %s", n.symbol ? strings.string_of(n.symbol.name) : "Unnamed"));
                write_named_children(node, "Type: ", "Expr: ");
            },
            (Definition* n) {
                // str.put(format(" %s", n.symbol ? strings.string_of(n.symbol.name) : "Unnamed"));
                write_named_children(node, "Type: ", "Expr: ");
            },
            (Variable* n) {
                // str.put(format(" %s", n.symbol ? strings.string_of(n.symbol.name) : "Unnamed"));
                write_named_children(node, "Type: ", "Expr: ");
            },
            (AstNode* n) {
                write_children(node);
            }
        );
    }

    void write_children(AstNodeId n, bool numbered = false) {
        str.put("\n");
        foreach (i, child; source.children_of(n))
            write_child(child, i + 1 == source.children_of(n).length, numbered ? format("#%s", i) : "");
    }

    void write_named_children(AstNodeId n, string[] names...)
    in(names.length == source.children_of(n).length) {
        str.put("\n");
        foreach (i, child; source.children_of(n)) {
            write_child(child, i + 1 == source.children_of(n).length, names[i]);
        }
    }

    void write_child(AstNodeId n, bool is_last_child, string prefix) {
        foreach (type; stack[])
            str.put(indent_str[type]);

        if (is_last_child) {
            str.put(lbar);
            stack.insertBack(IndentType.Space);
        }
        else {
            str.put(tbar);
            stack.insertBack(IndentType.Bar);
        }

        print(n, prefix);
        stack.removeBack();
    }
}
