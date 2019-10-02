module arc.syntax.ast;

/// Base class for the visitor platform
abstract class AstVisitor {

    /// Functions for visiting ast nodes
    void visit(Invalid n)       { visit_children(n); }
    /// ditto
    void visit(Name n)          { visit_children(n); }
    /// ditto
    void visit(Integer n)       { visit_children(n); }
    /// ditto
    void visit(List n)          { visit_children(n); }
    /// ditto
    void visit(Function n)      { visit_children(n); }
    /// ditto
    void visit(Negate n)        { visit_children(n); }
    /// ditto
    void visit(Add n)           { visit_children(n); }
    /// ditto
    void visit(Subtract n)      { visit_children(n); }
    /// ditto
    void visit(Multiply n)      { visit_children(n); }
    /// ditto
    void visit(Divide n)        { visit_children(n); }
    /// ditto
    void visit(Power n)         { visit_children(n); }
    /// ditto
    void visit(Call n)          { visit_children(n); }
    /// ditto
    void visit(VarExpression n) { visit_children(n); }

    void visit_children(AstNode n) {
        foreach (child; n.children)
            child.accept(this);
    }
}

/// Base class for the abstract syntax tree
abstract class AstNode {
    enum Type {
        Invalid,
        Name,
        Integer,
        List,
        Function,
        Negate,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Call,
        VarExpression,
    }

    alias Type this;

    /// The location of the start of the node's text
    const(char)* start;
    /// The length of the text represented by this node
    size_t span;
    ///
    Type type;

    this(Type type, const(char)* start, size_t span) {
        this.type = type;
        this.start = start;
        this.span = span;
    }

    /// Accept function of the Visitor Pattern's visit/accept pair
    abstract void accept(AstVisitor v);

    /// Retrieves the children of this node as a slice.
    AstNode[] children() { return []; }

    const(char)[] text() { return start[0 .. span]; }
}

abstract class Expression: AstNode {
    this(Type type, const(char)* start, size_t span) { super(type, start, span); }
}

final class Invalid: Expression {
    this(const(char)* start, size_t span) {
        super(Type.Invalid, start, span);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Name: Expression {
    this(const(char)* start, size_t span) {
        super(Type.Name, start, span);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Integer: Expression {
    private ulong _value;

    this(const(char)* start, size_t span, ulong value) {
        super(Type.Integer, start, span);
        _value = value;
    }

    override void accept(AstVisitor v) { v.visit(this); }

    ulong value() { return _value; }
}

/**
 * A List represents a possibly heterogenous sequence of ordered elements that 
 * may be referred to by numerical index or name.
 */
final class List: Expression {
    private VarExpression[] _members;

    this(const(char)* start, size_t span) {
        super(Type.List, start, span);
    }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return cast(AstNode[]) _members; }
    alias members = children;

    void children(VarExpression[] n) { _members = n; }

    /// Add a member to the list
    List add_member(VarExpression n) { _members ~= n; return this; }
}

final class Function: Expression {
    private Expression[2] _members;

    this(Expression params, Expression body) {
        super(Type.Function, params.start, (body.start + body.span) - params.start);
        _members = [params, body];
    }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return cast(AstNode[]) _members; }

    Expression parameters() { return _members[0]; }

    Expression body() { return _members[1]; }
}

final class Negate: Expression {
    private AstNode _expression;

    this(AstNode expr, const(char)* start, size_t span) {
        super(Type.Negate, start, span);
        _expression = expr;
    }

    override AstNode[] children() { return [_expression]; }

    override void accept(AstVisitor v) { v.visit(this); }
}

abstract class Binary: Expression {
    private Expression[2] _members;

    this(Type type, Expression left, Expression right) {
        super(type, left.start, (right.start + right.span) - left.start);
        _members = [left, right];
    }

    Expression left() { return _members[0]; }
    void left(Expression n) { _members[0] = n; }

    Expression right() { return _members[1]; }
    void right(Expression n) { _members[1] = n; }

    override AstNode[] children() { return cast(AstNode[]) _members; }
}

final class Add: Binary {
    this(Expression left, Expression right) {
        super(Type.Add, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Subtract: Binary {
    this(Expression left, Expression right) {
        super(Type.Subtract, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Multiply: Binary {
    this(Expression left, Expression right) {
        super(Type.Multiply, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Divide: Binary {
    this(Expression left, Expression right) {
        super(Type.Divide, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Power: Binary {
    this(Expression left, Expression right) {
        super(Type.Power, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Call: Binary {
    this(Expression target, Expression arguments) {
        super(Type.Call, target, arguments);
    }

    override void accept(AstVisitor v) { v.visit(this); }

    Expression target() { return _members[0]; }
    Expression arguments() { return _members[1]; }
}

/**
 * A variable declaration. This is used not only in let declarations, but also
 * to represent list members.
 *
 * In a list, a vardecl would typically just be a value, but it may also have
 * a name. In the particular case of a parameter list, it must have a name, and
 * possibly a type and/or a default value. If type is omitted, it must be
 * derivable either from the type of the default value, or from its context.
 *
 * VarDecl is also used in trees of `let` and `def` declarations.
 *
 * A `let` declaration must have a name, and either a type or a value, or both.
 * A `def` declaration must have a name, and either a type or a value, or both.
 * If the type is not present, it must be derivable.
 */
final class VarExpression: Expression {
    Expression[3] _members;

    /**
     * We include `const(char)* start` and `size_t span` because that is more
     * easily calculated by the parser and it saves us some effort here
     */
    this(Expression pattern, Expression type_expr, Expression value_expr, const(char)* start, size_t span) {
        super(Type.VarExpression, start, span);
        _members = [pattern, type_expr, value_expr];
    }

    override void accept(AstVisitor v) { v.visit(this); }
    override AstNode[] children() { return cast(AstNode[]) _members; }

    Expression pattern() { return _members[0]; }
    Expression type_expr() { return _members[1]; }
    Expression value_expr() { return _members[2]; }
}
