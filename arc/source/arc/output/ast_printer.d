module arc.output.ast_printer;

import arc.syntax.ast;

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
    import arc.syntax.ast: AstNode;
    import arc.syntax.location: SpannedText;

    enum IndentType: ubyte {
        None, Space, Bar
    }

    static immutable indent_str = ["NONE", "    ", " │  "];
    enum tbar = " ├─ ";
    enum lbar = " └─ ";

    Array!IndentType stack;
    Appender!(char[]) str;
    SpannedText text;
    const(char)[] name_override;

    this(SpannedText text) {
        str = appender!(char[]);
        this.text = text;
    }

    const(char)[] data() {
        return cast(const(char)[]) str.data;
    }

    void reset() {
        str.clear();
        name_override = "";
    }

    void print(AstNode n) {
        str.put(repr(n));
        switch (n.type) with (AstNode.Type) {
        case Invalid:
            str.put(" \"");
            str.put(text.get_text(n.span));
            str.put("\"\n");
            write_children(n);
            break;
        case List:
            str.put(" (");
            str.put(n.num_children.to!string);
            str.put(")\n");
            write_children(n, true);
            break;
        case Function:
            str.put(" (");
            str.put(n.num_children.to!string);
            str.put(")\n");
            write_named_children(n, "Params: ", "Body: ");
            break;
        case Call:
            str.put(" (");
            str.put(n.num_children.to!string);
            str.put(")\n");
            write_named_children(n, "Target: ", "Members: ");
            break;
        case VarExpression:
            str.put("\n");
            write_named_children(n, "Pattern: ", "Type: ", "Value: ");
            break;
        default:
            str.put("\n");
            write_children(n);
        }
    }

    void write_children(AstNode n, bool numbered = false) {
        if (n.num_children == 0)
            return;
        
        foreach (i, child; n.children) {
            indent();

            if (i + 1 == n.num_children) {
                // this is the last child
                str.put(lbar);
                stack.insertBack(IndentType.Space);
            }
            else {
                str.put(tbar);
                stack.insertBack(IndentType.Bar);
            }

            if (numbered)
                name_override = "#" ~ i.to!string ~ " ";
            print(child);
            stack.removeBack();
        }
    }

    void write_named_children(AstNode n, string[] names...) {
        assert(n.num_children == names.length);

        if (n.num_children == 0)
            return;
        
        foreach (i, child; n.children) {
            indent();

            if (i + 1 == n.num_children) {
                // this is the last child
                str.put(lbar);
                stack.insertBack(IndentType.Space);
            }
            else {
                str.put(tbar);
                stack.insertBack(IndentType.Bar);
            }

            name_override = names[i];
            print(child);
            stack.removeBack();
        }
    }

    void indent() {
        foreach (type; stack[])
            str.put(indent_str[type]);
    }

    const(char)[] repr(AstNode node) {
        import std.conv: to;

        scope(exit) name_override = "";

        if (node is null)
            return name_override ~ "Null";

        switch (node.type) with (AstNode.Type) {
            case Name:
                return name_override ~ "Name(\"" ~ text.get_text(node.span) ~ "\")";
            case Integer:
            case Char:
                return name_override ~ text.get_text(node.span);
            default:
                return name_override ~ node.type.to!string;
        }
    }
}