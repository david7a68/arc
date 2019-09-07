module arc.syntax.ast;

/// Base class for the visitor platform
abstract class AstVisitor {

    /// Functions for visiting ast nodes
    void visit(Invalid n)   { visit_children(n); }
    /// ditto
    void visit(Tuple n)     { visit_children(n); }
    /// ditto
    void visit(Integer n)   { visit_children(n); }
    /// ditto
    void visit(Name n)      { visit_children(n); }

    void visit_children(AstNode n) {
        foreach (child; n.children)
            child.accept(this);
    }
}

/// Base class for the abstract syntax tree
abstract class AstNode {
    enum Type {
        Invalid,
        Tuple,
        Integer,
        Name
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

final class Invalid: AstNode {
    this() { super(Type.Invalid); }
}

abstract class Expression: AstNode {
    this(Type type) { super(type); }
}

final class Tuple: Expression {
    private AstNode[] _members;

    this() { super(Type.Tuple); }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return _members; }

    /// Add a member to the tuple
    Tuple add_member(AstNode n) { _members ~= n; return this; }
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
