module arc.output.ast_printer;

import arc.data.ast;
import arc.data.stringtable;
import std.format: format;

const(char[]) print_ast(StringTable* strings, AstNode*[] nodes...) {
    auto printer = AstPrinter(strings);

    foreach (node; nodes)
        printer.print(node);

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
    StringTable* strings;

    this(StringTable* strings) {
        str = appender!(char[]);
        this.strings = strings;
    }

    const(char)[] data() const {
        return str.data;
    }

    void reset() {
        str.clear();
    }

    void print(AstNode* node, string prefix = "") {
        str.put(prefix);

        str.put(node.match!(const(char)[])(
            (CharLiteral* n) => format("%s (%s)", n.kind, strings.string_of(n.value)),
            (StrLiteral* n) => format("%s (%s)", n.kind, strings.string_of(n.value)),
            (IntLiteral* n) => format("%s (%s)", n.kind, n.value),
            (SymbolRef* n) => format("%s (%s)", n.kind, strings.string_of(n.text)),
            (AstNode* n) => format("%s", n.kind)
        ));

        node.match!void(
            (Function* n) {
                write_named_children(n.header, "Params: ", "Return Type: ", "Body: ");
            },
            (List* n) {
                str.put(format(" (%s)", node.children.length));
                write_children(n.header, true);
            },
            (UnOp *n) {
                write_named_children(n.header, "Operand: ");
            },
            (BinOp *n) {
                if (n.kind == AstNode.Kind.Call)
                    write_named_children(n.header, "Target: ", "Arguments: ");
                else if (n.kind == AstNode.Kind.Access || n.kind == AstNode.Kind.StaticAccess)
                    write_named_children(n.header, "Source: ", "Member: ");
                else
                    write_named_children(n.header, "Left: ", "Right: ");
            },
            (If* n) {
                write_named_children(n.header, "Condition: ", "Body: ", "Else: ");
            },
            (Declaration* n) {
                str.put(format(" %s", n.symbol ? strings.string_of(n.symbol.name) : "Unnamed"));
                write_named_children(n.header, "Type: ", "Expr: ");
            },
            (AstNode* n) {
                write_children(n);
            }
        );
    }

    void write_children(AstNode* n, bool numbered = false) {
        str.put("\n");
        foreach (i, child; n.children)
            write_child(child, i + 1 == n.children.length, numbered ? format("#%s", i) : "");
    }

    void write_named_children(AstNode* n, string[] names...)
    in(names.length == n.children.length) {
        str.put("\n");
        foreach (i, child; n.children) {
            write_child(child, i + 1 == n.children.length, names[i]);
        }
    }

    void write_child(AstNode* n, bool is_last_child, string prefix) {
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
