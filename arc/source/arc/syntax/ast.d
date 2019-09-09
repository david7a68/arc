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
    void visit(Tuple n)         { visit_children(n); }
    /// ditto
    void visit(Vector n)        { visit_children(n); }
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
        Tuple,
        Vector,
        Negate,
        Add,
        Subtract,
        Multiply,
        Divide,
        Call,
    }

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

final class Integer: Expression {
    private ulong _value;

    this(const(char)* start, size_t span) {
        super(Type.Integer);
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

final class Tuple: Expression {
    private AstNode[] _members;

    this() { super(Type.Tuple); }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return _members; }

    /// Add a member to the tuple
    Tuple add_member(AstNode n) { _members ~= n; return this; }
}

final class Vector: Expression {
    private AstNode[] _members;

    this() { super(Type.Vector); }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return _members; }

    /// Add a member to the vector
    Vector add_member(AstNode n) { _members ~= n; return this; }
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
    private AstNode[2] _members;

    this(Type type, AstNode left, AstNode right) {
        super(type);
        _members = [left, right];
    }

    AstNode left() { return _members[0]; }
    void left(AstNode n) { _members[0] = n; }

    AstNode right() { return _members[1]; }
    void right(AstNode n) { _members[1] = n; }

    override AstNode[] children() { return _members; }
}

final class Add: Binary {
    this(AstNode left, AstNode right) {
        super(Type.Add, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Subtract: Binary {
    this(AstNode left, AstNode right) {
        super(Type.Subtract, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Multiply: Binary {
    this(AstNode left, AstNode right) {
        super(Type.Multiply, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Divide: Binary {
    this(AstNode left, AstNode right) {
        super(Type.Divide, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Call: Binary {
    this(AstNode left, AstNode right) {
        super(Type.Call, left, right);
    }

    override void accept(AstVisitor v) { v.visit(this); }
}
