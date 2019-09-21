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
    void visit(List n)         { visit_children(n); }
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
        Negate,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Call,
    }

    alias Type this;

    /// The location of the start of the node's text
    const(char)* start;
    /// The length of the text represented by this node
    size_t span;
    ///
    Type type;

    this(Type type) { this.type = type; }

    /// Accept function of the Visitor Pattern's visit/accept pair
    abstract void accept(AstVisitor v);

    /// Retrieves the children of this node as a slice.
    AstNode[] children() { return []; }
}

abstract class Expression: AstNode {
    this(Type type) { super(type); }
}

final class Invalid: Expression {
    this() { super(Type.Invalid); }

    this(const(char)* start, size_t span) {
        this();
        this.start = start;
        this.span = span;
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Name: Expression {
    this(const(char)* start, size_t span) {
        super(Type.Name);
        this.start = start;
        this.span = span;
    }

    /// The text represented by this name
    const(char)[] text() { return start[0 .. span]; }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Integer: Expression {
    private ulong _value;

    this(const(char)* start, size_t span) {
        super(Type.Integer);
        this.start = start;
        this.span = span;
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

/**
 * A List represents a possibly heterogenous sequence of ordered elements that 
 * may be referred to by numerical index or name.
 */
final class List: Expression {
    private AstNode[] _members;

    this(const(char)* start, size_t span) {
        super(Type.List);
        this.start = start;
        this.span = span;
    }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return _members; }

    void children(AstNode[] n) { _members = n; }

    /// Add a member to the list
    List add_member(AstNode n) { _members ~= n; return this; }
}

final class Negate: Expression {
    private AstNode _expression;

    this(AstNode expr) {
        super(Type.Negate);
        _expression = expr;
    }

    override AstNode[] children() { return [_expression]; }

    override void accept(AstVisitor v) { v.visit(this); }
}

abstract class Binary: Expression {
    private Expression[2] _members;

    this(Type type, Expression left, Expression right) {
        super(type);
        _members = [left, right];
        start = left.start;
        span = (right.start + right.span) - left.start;
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
