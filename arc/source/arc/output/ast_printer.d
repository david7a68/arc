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
    import arc.syntax.location: SpannedText;

    /// Initialize the printer
    this(SpannedText text) {
        str = appender!(char[]);
        this.text = text;
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
        str.put(text.get_text(n.span));
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
        write(n.children[0]);
        stack.removeBack();

        indent();
        str.put(lbar);
        stack.insertBack(IndentType.Space);
        name_override = "Members: ";
        write(n.children[1]);
        stack.removeBack();
    }

    override void visit(VarExpression n) {
        str.put(repr(n));
        str.put("\n");

        indent();
        str.put(tbar);
        stack.insertBack(IndentType.Bar);
        name_override = "Pattern: ";
        write(n.pattern);
        stack.removeBack();

        indent();
        str.put(tbar);
        stack.insertBack(IndentType.Bar);
        name_override = "Type: ";
        write(n.type_expr);
        stack.removeBack();

        indent();
        str.put(lbar);
        stack.insertBack(IndentType.Space);
        name_override = "Value: ";
        write(n.value_expr);
        stack.removeBack();
    }

    void write(AstNode n) {
        str.put(repr(n));
        str.put("\n");

        if (n) write_children(n);
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
    SpannedText text;
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
        import std.conv: to;

        scope(exit) name_override = "";

        if (node is null)
            return name_override ~ "Null";

        switch (node.type) with (AstNode.Type) {
            case Name:
            case Integer:
                return name_override ~ text.get_text(node.span);
            default:
                return name_override ~ node.type.to!string;
        }
    }
}
