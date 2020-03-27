module arc.output.ast_printer;

import arc.syntax.ast;
import arc.source: SourceMap;

/**
 * AstVisitor that generates a string representation of the syntax tree.
 *
 * Formats an expression as an expanding tree to look something like the following:
 *
 *  ```
 *  let a = (b-(square 19), some_var)
 *  ```
 *
 *  becomes
 *
 *  ```
 *  Let
 *   ├─ a
 *   ├─ Null
 *   └─ Group
 *       ├─ Binary
 *       │    ├─ -
 *       │    ├─ b
 *       │    └─ Unary
 *       │        ├─ square
 *       │        └─ 19
 *       └─ Null
 *  ```
 */
struct AstPrinter {
    import std.array: appender, Appender;
    import std.container.array: Array;
    import std.conv: to;

    enum IndentType: ubyte {
        None, Space, Bar
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

    const(char)[] data() {
        return cast(const(char)[]) str.data;
    }

    void reset() {
        str.clear();
    }

    void print(AstNode* n, string prefix = "") {
        void put_length() {
            str.put(" (");
            str.put(n.get_children().length.to!string);
            str.put(")");
        }

        str.put(prefix);
        str.put(repr(sources, n));
        switch (n.type) with (AstNode.Type) {
        case Invalid:
            str.put(" \"");
            str.put(sources.get_spanned_text(n.span));
            str.put("\"");
            write_children(n);
            break;
        case List:
        case TypeList:
            put_length();
            write_children(n, true);
            break;
        case ListMember:
            write_named_children(n, "Name: ", "Type: ", "Value: ");
            break;
        case TypeListMember:
            write_named_children(n, "Name: ", "Type: ");
            break;
        case Function:
            write_named_children(n, "Params: ", "Return Type: ", "Body: ");
            break;
        case Call:
            write_named_children(n, "Target: ", "Arguments: ");
            break;
        case Define:
            write_named_children(n, "Name: ", "Type: ", "Value: ");
            break;
        case Variable:
            write_named_children(n, "Pattern: ", "Type: ", "Value: ");
            break;
        case If:
            write_named_children(n, "Cond: ", "Body: ", "Else: ");
            break;
        case Return:
            write_named_children(n, "Value: ");
            break;
        default:
            write_children(n);
        }
    }

    void write_children(AstNode* n, bool numbered = false) {
        str.put("\n");
        foreach (i, child; n.get_children()) {
            write_child(child, i + 1 == n.get_children().length, numbered ? ("#" ~ i.to!string ~ " ") : "");
        }
    }

    void write_named_children(AstNode* n, string[] names...) {
        assert(n.get_children().length == names.length);
        
        str.put("\n");
        foreach (i, child; n.get_children()) {
            write_child(child, i + 1 == n.get_children().length, names[i]);
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
    import std.conv: to;

    switch (node.type) with (AstNode.Type) {
        case Name:
            return "Name(\"" ~ sources.get_spanned_text(node.span) ~ "\")";
        case Integer:
        case Char:
            return sources.get_spanned_text(node.span);
        default:
            return node.type.to!string;
    }
}