module arc.output.ast_printer;

import arc.syntax.ast: AstNode, AstNodeVisitor;

/**
 * Returns the string representation of an AST node
 */
const(char)[] repr(AstNode* node) {
    switch (node.type) with (AstNode.Type) {
        case Invalid:
            return "Invalid";
        case Tuple:
            return "Tuple";
        case Name:
        case Integer:
            return node.start[0 .. node.span];
        default:
            assert(false);
    }
}

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
final class AstPrinter: AstNodeVisitor {
    import std.container.array: Array;
    import std.array: appender, Appender;

    /// Initialize the printer
    this() {
        str = appender!(char[]);
    }

    /**
     * Accesses the string managed by the printer
     */
    string data() {
        return str.data;
    }

    import std.traits: EnumMembers;
    static foreach(member; EnumMembers!(AstNode.Type)) {
        import std.format: fmt = format;
        import std.conv: to;
        import std.uni: toLower;

        mixin("override void visit_%s(AstNode* n) {
            str.put(repr(n));
            str.put(\"\\n\");

            if (n.first_child !is null)
                write_and_adjescent(n.first_child);
        }".fmt(member.to!string.toLower));
    }

    /**
     * Resets the printer to accept another tree for printing.
     *
     * If you want to keep the result returned by `data`, make sure to duplicate
     * it first.
     */
    void reset() {
        str.clear();
    }

private:
    enum IndentType: ubyte {
        None, Space, Bar
    }

    static immutable indent_str = ["NONE", "    ", " │  "];
    enum tbar = " ├─ ";
    enum lbar = " └─ ";

    Array!IndentType stack;
    Appender!(char[]) str;

    void write_and_adjescent(AstNode* n) {
        const first = n;
        auto next = n;
        do {
            auto current = next;
            next = next.next;

            indent();

            if (next is first) {
                str.put(lbar);
                stack.insertBack(IndentType.Space);
            }
            else {
                str.put(tbar);
                stack.insertBack(IndentType.Bar);
            }

            if (current.first_child !is null)
                current.first_child.accept(this);
            else str.put("Null\n");
            stack.removeBack();
        } while (next !is first);
    }

    void indent() {
        foreach (type; stack[])
            str.put(indent_str[type]);
    }
}
