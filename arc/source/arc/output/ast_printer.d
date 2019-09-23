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
final class AstPrinter: AstVisitor {
    import std.array: appender, Appender;
    import std.container.array: Array;
    import std.conv: to;

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

    override void visit(Invalid n)  {
        str.put(repr(n));
        str.put(" \"");
        str.put(n.start[0 .. n.span]);
        str.put("\"\n");
        write_children(n);
    }

    override void visit(Name n)     { write(n); }
    override void visit(Integer n)  { write(n); }
    
    override void visit(List n) {
        str.put(repr(n));
        str.put(" (");
        str.put(n.children.length.to!string);
        str.put(")\n");

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

            name_override = "#" ~ i.to!string ~ " ";
            child.accept(this);
            stack.removeBack();
        }
    }
    
    override void visit(Function n) {
        str.put(repr(n));
        str.put(" (");
        str.put(n.children.length.to!string);
        str.put(")\n");

        indent();
        str.put(tbar);
        stack.insertBack(IndentType.Bar);
        name_override = "Params: ";
        n.children[0].accept(this);
        stack.removeBack();

        indent();
        str.put(lbar);
        stack.insertBack(IndentType.Space);
        name_override = "Body: ";
        n.children[1].accept(this);
        stack.removeBack();
    }

    override void visit(Negate n)   { write(n); }
    override void visit(Add n)      { write(n); }
    override void visit(Subtract n) { write(n); }
    override void visit(Multiply n) { write(n); }
    override void visit(Divide n)   { write(n); }
    override void visit(Power n)    { write(n); }

    override void visit(Call n) {
        str.put(repr(n));
        str.put(" (");
        str.put(n.children[1].children.length.to!string);
        str.put(")\n");

        indent();
        str.put(tbar);
        stack.insertBack(IndentType.Bar);
        name_override = "Target: ";
        n.children[0].accept(this);
        stack.removeBack();

        indent();
        str.put(lbar);
        stack.insertBack(IndentType.Space);
        name_override = "Members: ";
        n.children[1].accept(this);
        stack.removeBack();
    }

    void write(AstNode n) {
        str.put(repr(n));
        str.put("\n");
        write_children(n);
    }

    /**
     * Resets the printer to accept another tree for printing.
     *
     * If you want to keep the result returned by `data`, make sure to duplicate
     * it first.
     */
    void reset() {
        str.clear();
        name_override = "";
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
    const(char)[] name_override;

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

    /**
    * Returns the string representation of an AST node
    */
    const(char)[] repr(AstNode node) {
        scope(exit) name_override = "";

        switch (node.type) with (AstNode.Type) {
            case Invalid:
                return "Invalid";
            case Name:
            case Integer:
                return name_override ~ node.start[0 .. node.span];
            case List:
                return name_override ~ "List";
            case Function:
                return name_override ~ "Function";
            case Negate:
                return name_override ~ "Negate";
            case Add:
                return name_override ~ "Add";
            case Subtract:
                return name_override ~ "Subtract";
            case Multiply:
                return name_override ~ "Multiply";
            case Divide:
                return name_override ~ "Divide";
            case Power:
                return name_override ~ "Power";
            case Call:
                return name_override ~ "Call";
            default:
                assert(false);
        }
    }
}

version(unittest):

immutable test_result = "List (3)
 ├─ #0 a
 ├─ #1 b
 └─ #2 List (2)
     ├─ #0 c
     └─ #1 102
";

unittest {
    const s = "a b c 102";

    auto tup = new List(&s[0], s.length);
    tup.add_member(new Name(&s[0], 1));
    tup.add_member(new Name(&s[2], 1));
    tup.add_member(new List(&s[4], 5)
                    .add_member(new Name(&s[4], 1))
                    .add_member(new Integer(&s[6], 3, 102)));

    auto p = new AstPrinter;
    p.visit(tup);
    assert(p.data == test_result);

    p.reset();

    p.visit(tup);
    assert(p.data == test_result);
}