module arc.syntax.ast;

import arc.syntax.location: Span;

/// Base class for the visitor platform
abstract class AstVisitor {

    /// Functions for visiting ast nodes
    void visit(Invalid n)       { visit_children(n); }
    /// ditto
    void visit(None n)          { visit_children(n); }
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
    /// ditto
    void visit(Define n)        { visit_children(n); }

    void visit_children(AstNode n) {
        foreach (child; n.children)
            child.accept(this);
    }
}

/// Base class for the abstract syntax tree
abstract class AstNode {
    enum Type {
        Invalid,
        None,
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
        Define
    }

    alias Type this;

    /// The location of the start of the node's text
    // const(char)* start;
    /// The length of the text represented by this node
    // size_t span;
    Span span;

    ///
    Type type;

    this(Type type, Span span) {
        this.type = type;
        this.span = span;
    }

    /// Accept function of the Visitor Pattern's visit/accept pair
    abstract void accept(AstVisitor v);

    /// Retrieves the children of this node as a slice.
    AstNode[] children() { return []; }
}

abstract class Expression: Statement {
    this(Type type, Span span) { super(type, span); }
}

final class Invalid: Expression {
    AstNode[] _children;

    this(Span span, AstNode[] children = []) {
        super(Type.Invalid, span);
        _children = children;
    }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return _children; }
}

final class None: Expression {
    private this() {
        super(Type.None, Span(0, 0));
    }

    override void accept(AstVisitor v) { v.visit(this); }

    static instance() {
        static immutable instance = cast(immutable) new None();
        return cast(None) instance;
    }
}

final class Name: Expression {
    import arc.hash: Key;

    Key key;

    this(Span span, Key key) {
        super(Type.Name, span);
        this.key = key;
    }

    override void accept(AstVisitor v) { v.visit(this); }
}

final class Integer: Expression {
    private ulong _value;

    this(Span span, ulong value) {
        super(Type.Integer, span);
        _value = value;
    }

    override void accept(AstVisitor v) { v.visit(this); }

    ulong value() const { return _value; }
}

/**
 * A List represents a possibly heterogenous sequence of ordered elements that 
 * may be referred to by numerical index or name.
 */
final class List: Expression {
    private VarExpression[] _members;

    this(Span span, VarExpression[] members) {
        super(Type.List, span);
        _members = members;
    }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return cast(AstNode[]) _members; }
    alias members = children;
}

final class Function: Expression {
    private Expression[2] _members;

    this(Expression params, Expression body) {
        super(Type.Function, params.span.merge(body.span));
        _members = [params, body];
    }

    override void accept(AstVisitor v) { v.visit(this); }

    override AstNode[] children() { return cast(AstNode[]) _members; }

    Expression parameters() { return _members[0]; }

    Expression body() { return _members[1]; }
}

final class Negate: Expression {
    private AstNode _expression;

    this(AstNode expr, Span span) {
        super(Type.Negate, span);
        _expression = expr;
    }

    override AstNode[] children() { return [_expression]; }

    override void accept(AstVisitor v) { v.visit(this); }
}

abstract class Binary: Expression {
    private Expression[2] _members;

    this(Type type, Expression left, Expression right) {
        super(type, left.span.merge(right.span));
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

    this(Expression pattern, Expression type_expr, Expression value_expr, Span span) {
        super(Type.VarExpression, span);
        _members = [pattern, type_expr, value_expr];
    }

    override void accept(AstVisitor v) { v.visit(this); }
    override AstNode[] children() { return cast(AstNode[]) _members; }

    Expression pattern() { return _members[0]; }
    Expression type_expr() { return _members[1]; }
    Expression value_expr() { return _members[2]; }
}

abstract class Statement: AstNode {
    this(AstNode.Type type, Span span) {
        super(type, span);
    }
}

final class Define: Statement {
    VarExpression var_expr;

    this(VarExpression expr, Span span) {
        super(Type.Define, span);
        var_expr = expr;
    }

    override void accept(AstVisitor v) { v.visit(this); }
    override AstNode[] children() { return [var_expr]; };
}
