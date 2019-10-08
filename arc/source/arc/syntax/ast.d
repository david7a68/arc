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
        Block,
        Function,
        Negate,
        SelfCall,
        Assign,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Call,
        Path,
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

static AstNode make_char(Span span) {
    return new AstNode(AstNode.Char, span);
}

alias make_name = make_value!("key", Key, AstNode.Name);
alias make_int = make_value!("value", ulong, AstNode.Integer);

static AstNode make_value(string name, T, AstNode.Type t)(Span span, T value) {
    auto node = new AstNode(t, span);
    mixin("node." ~ name ~ " = value;");
    return node;
}

static AstNode make_binary(AstNode.Type t, AstNode left, AstNode right) {
    return make_n_ary(t, left.span.merge(right.span), left, right);
}

static AstNode make_n_ary(AstNode.Type t, Span span, AstNode[] nodes...) {
    auto node = new AstNode(t, span);
    node.children = nodes.dup;
    return node;
}
