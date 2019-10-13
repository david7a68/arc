module arc.syntax.ast;

import arc.hash: Key;
import arc.syntax.location: Span;

struct AstNode {
    enum Type {
        Invalid,
        None,
        Name,
        Integer,
        Char,
        List,
        Array,
        Block,
        Function,
        Negate,
        SelfCall,
        Pointer,
        GetRef,
        Assign,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,
        Equal,
        NotEqual,
        And,
        Or,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Call,
        Path,
        VarExpression,
        Define,
        If,
        Loop,
        Label,
        Break,
        Return,
        Continue,
        Labeled,
    }

    alias type this;

    Type type;
    Span span;

    union {
        Key key;
        ulong value;
        AstNode*[] children;
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
            case Label:
                return 0;
            default:
                return children.length;
        }
    }

    static immutable none_value = AstNode(AstNode.None, Span());
    static AstNode* none() { return cast(AstNode*) &none_value; }
}

AstNode* make_value(string name, T, AstNode.Type t)(Span span, T value) {
    auto node = new AstNode(t, span);
    mixin("node." ~ name ~ " = value;");
    return node;
}

AstNode* make_binary(AstNode.Type t, AstNode* left, AstNode* right) {
    return make_n_ary(t, left.span.merge(right.span), left, right);
}

AstNode* make_n_ary(AstNode.Type t, Span span, AstNode*[] nodes...) {
    auto node = new AstNode(t, span);
    node.children = nodes.dup;
    return node;
}

AstNode* make_invalid(Span span) {
    return new AstNode(AstNode.Invalid, span);
}

alias make_name = make_value!("key", Key, AstNode.Name);
alias make_int = make_value!("value", ulong, AstNode.Integer);
alias make_label = make_value!("key", Key, AstNode.Label);

AstNode* make_char(Span span) {
    return new AstNode(AstNode.Char, span);
}
