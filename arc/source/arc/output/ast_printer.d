module arc.output.ast_printer;

import arc.syntax.ast;
import arc.syntax.location: SpannedText;

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
    SpannedText text;

    this(SpannedText text) {
        str = appender!(char[]);
        this.text = text;
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
            str.put(n.num_children.to!string);
            str.put(")");
        }

        str.put(prefix);
        str.put(repr(text, n));
        switch (n.type) with (AstNode.Type) {
        case Invalid:
            str.put(" \"");
            str.put(text.get_text(n.span));
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
        case VarExpression:
            write_named_children(n, "Pattern: ", "Type: ", "Value: ");
            break;
        case Define:
            write_named_children(n, "Name: ", "Type: ", "Value: ");
            break;
        case If:
            write_named_children(n, "Cond: ", "Body: ", "Else: ");
            break;
        case Return:
        case Break:
            write_named_children(n, "Label: ", "Value: ");
            break;
        case Continue:
            write_named_children(n, "Label: ");
            break;
        default:
            write_children(n);
        }
    }

    void write_children(AstNode* n, bool numbered = false) {
        str.put("\n");
        if (n.num_children > 0) {
            foreach (i, child; n.children) {
                write_child(child, i + 1 == n.num_children, numbered ? ("#" ~ i.to!string ~ " ") : "");
            }
        }
    }

    void write_named_children(AstNode* n, string[] names...) {
        assert(n.num_children == names.length);
        
        str.put("\n");
        if (n.num_children > 0) {
            foreach (i, child; n.children) {
                write_child(child, i + 1 == n.num_children, names[i]);
            }
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

const(char)[] repr(SpannedText text, AstNode* node) {
    import std.conv: to;

    switch (node.type) with (AstNode.Type) {
        case Name:
            return "Name(\"" ~ text.get_text(node.span) ~ "\")";
        case Label:
            return "Label: " ~ text.get_text(node.span);
        case Integer:
        case Char:
            return text.get_text(node.span);
        default:
            return node.type.to!string;
    }
}