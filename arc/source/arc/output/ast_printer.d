module arc.output.ast_printer;

import arc.syntax.ast;

/**
 * Returns the string representation of an AST node
 */
const(char)[] repr(AstNode node) {
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
final class AstPrinter: AstVisitor {
    import std.container.array: Array;
    import std.array: appender, Appender;

    /// Initialize the printer
    this() {
        str = appender!(char[]);
    }

    /**
     * Accesses the string managed by the printer
     */
    const(char)[] data() {
        return cast(const(char)[]) str.data;
    }

    import std.traits: EnumMembers;
    static foreach(member; EnumMembers!(AstNode.Type)) {
        import std.format: format;
        import std.conv: to;

        mixin("override void visit(%s n) {
            str.put(repr(n));
            str.put(\"\\n\");

            write_children(n);
        }".format(member.to!string));
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

    void write_children(AstNode n) {
        foreach (i, child; n.children) {
            indent();

            if (i + 1 == n.children.length) {
                // this is the last child
                str.put(lbar);
                stack.insertBack(IndentType.Space);
            }
            else {
                str.put(tbar);
                stack.insertBack(IndentType.Bar);
            }

            child.accept(this);
            stack.removeBack();
        }
    }

    void indent() {
        foreach (type; stack[])
            str.put(indent_str[type]);
    }
}

version(unittest):

immutable test_result = "Tuple
 ├─ a
 ├─ b
 └─ Tuple
     ├─ c
     └─ 102
";

unittest {
    const s = "a b c 102";

    auto tup = new Tuple();
    tup.add_member(new Name(&s[0], 1));
    tup.add_member(new Name(&s[2], 1));
    tup.add_member(new Tuple()
                    .add_member(new Name(&s[4], 1))
                    .add_member(new Integer(&s[6], 3)));

    auto p = new AstPrinter;
    p.visit(tup);
    assert(p.data == test_result);

    p.reset();

    p.visit(tup);
    assert(p.data == test_result);
}