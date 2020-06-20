module arc.output.ast_printer;

import arc.data.ast;
import arc.data.source_map;
import std.conv : to;

const(char[]) print_ast(SourceMap sources, AstNode*[] nodes...) {
    auto printer = AstPrinter(sources);

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
 *       │    ├─ Name: None
 *       │    ├─ Type: Inferred
 *       |    └─ Expr: -
 *       │       ├─ b
 *       │       └─ square
 *       │          └─ 19
 *       └─ ListMember
 *            ├─ None
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
    SourceMap sources;

    this(SourceMap sources) {
        str = appender!(char[]);
        this.sources = sources;
    }

    const(char)[] data() const {
        return str.data;
    }

    void reset() {
        str.clear();
    }

    void print(AstNode* n, string prefix = "") {
        void put_length() {
            str.put(" (");
            str.put(n.children.length.to!string);
            str.put(")");
        }

        str.put(prefix);
        str.put(repr(sources, n));
        switch (n.kind) with (AstNode.Kind) {
        case Invalid:
            str.put(" \"");
            str.put(sources.get_spanned_text(n.span));
            str.put("\"\n");
            break;
        case List:
            put_length();
            write_children(n, true);
            break;
        case Function:
            write_named_children(n, "Params: ", "Return Type: ", "Body: ");
            break;
        case Call:
            write_named_children(n, "Target: ", "Arguments: ");
            break;
        case StaticAccess:
        case Access:
            write_named_children(n, "Source: ", "Member: ");
            break;
        case Definition:
        case Variable:
            // str.put(" (");
            // str.put(n.children[0].symbol
            //         ? n.children[0].symbol.kind.to!string : "Unresolved");
            // str.put(")");
            write_named_children(n, "Name: ", "Type: ", "Expr: ");
            break;
        case If:
            write_named_children(n, "Condition: ", "Body: ", "Else: ");
            break;
        case Negate:
        case Not:
            write_named_children(n, "Operand: ");
            break;
        case Add:
        case Subtract:
        case Multiply:
        case Divide:
        case Power:
        case Less:
        case LessEqual:
        case Greater:
        case GreaterEqual:
        case Equal:
        case NotEqual:
        case And:
        case Or:
        case Assign:
            write_named_children(n, "Left: ", "Right: ");
            break;
        default:
            write_children(n);
        }
    }

    void write_children(AstNode* n, bool numbered = false) {
        str.put("\n");
        foreach (i, child; n.children) {
            write_child(child, i + 1 == n.children.length, numbered ? ("#" ~ i.to!string ~ " ") : "");
        }
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

const(char)[] repr(SourceMap sources, AstNode* node) {
    switch (node.kind) with (AstNode.Kind) {
    case Name:
    case Integer:
    case Char:
        return node.kind.to!string ~ "(\"" ~ sources.get_spanned_text(node.span) ~ "\")";
    case String:
        return node.kind.to!string ~ "(" ~ sources.get_spanned_text(node.span) ~ ")";
    default:
        return node.kind.to!string;
    }
}
