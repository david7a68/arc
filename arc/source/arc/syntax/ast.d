module arc.syntax.ast;

import arc.hash: Key;
import arc.syntax.location: Span;

class AstNode {
    enum Type {
        Invalid,
        None,
        Name,
        Integer,
        Char,
        List,
        Function,
        Block,
        Negate,
        SelfCall,
        Assign,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Call,
        VarExpression,
        Define
    }

    alias type this;

    Type type;
    Span span;

    union {
        Key key;
        ulong value;
        AstNode[] children;
    }

    this(Type type, Span span) {
        this.type = type;
        this.span = span;
    }

    size_t num_children() {
        switch (type) with (Type) {
            case None:
            case Name:
            case Integer:
            case Char:
                return 0;
            default:
                return children.length;
        }
    }

    private static immutable _none = cast(immutable) new AstNode(AstNode.None, Span(0, 0));

    static AstNode none() { return cast(AstNode) _none; }
}

static AstNode make_invalid(Span span) {
    return new AstNode(AstNode.Invalid, span);
}

static AstNode make_name(Span span, Key key) {
    auto node = new AstNode(AstNode.Name, span);
    node.key = key;
    return node;
}

static AstNode make_int(Span span, ulong value) {
    auto node = new AstNode(AstNode.Integer, span);
    node.value = value;
    return node;
}

static AstNode make_char(Span span) {
    return new AstNode(AstNode.Char, span);
}

static AstNode make_binary(AstNode.Type t, AstNode left, AstNode right) {
    auto node = new AstNode(t, left.span.merge(right.span));
    node.children = [left, right];
    return node;
}

static AstNode make_n_ary(AstNode.Type t, Span span, AstNode[] nodes...) {
    return make_list(t, span, nodes.dup);
}

static AstNode make_list(AstNode.Type t, Span span, AstNode[] nodes) {
    auto node = new AstNode(t, span);
    node.children = nodes.dup;
    return node;
}
