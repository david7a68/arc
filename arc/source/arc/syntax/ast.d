module arc.syntax.ast;

import arc.hash: Key;
import arc.source: Span;

struct AstNode {
    enum Type : ubyte {
        Invalid,
        None,
        Module,
        // Statements
        Define,
        If,
        Loop,
        Break,
        Return,
        Continue,
        // Expressions
        Name,
        Integer,
        Char,
        List,
        ListMember,
        Block,
        Negate,
        Falsify,
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
        Access,
        Function,
        Variable,
        // Type expressions
        InferredType,
        PointerType,
        TypeList,
        TypeListMember,
        FunctionType
    }

    alias Type this;

    const Type type;
    const Span span;

    union {
        Key _value_key;
        AstNode* _child;
        AstNode*[] _children;
    }

    this(Type type, Span span) {
        this.type = type;
        this.span = span;
    }

    this(Type type, Span span, Key key) {
        this(type, span);
        _value_key = key;
    }

    this(Type type, Span span, AstNode* child) {
        this(type, span);
        _child = child;
    }

    this(Type type, Span span, AstNode*[] children) {
        this(type, span);
        _children = children;
    }

    Key get_key() {
        switch (type) with (Type) {
        case Name:
        case Char:
        case Integer:
            return _value_key;
        default:
            return 0;
        }
    }

    AstNode*[] get_children() {
        switch (type) with (Type) {
        case None:
        case Name:
        case Char:
        case Integer:
        case Break:
        case Continue:
        case InferredType:
            return [];
        case Return:
        case Loop:
        case Negate:
        case Falsify:
        case GetRef:
        case Pointer:
        case PointerType:
            return [_child];
        default:
            return _children;
        }
    }

    // These are pointers because we aren't copying these all over the place
    static AstNode* none;
    static AstNode* inferred_type;

    bool is_marker() {
        return type == Type.None || type == Type.InferredType;
    }
}

static this() {
    AstNode.none = new AstNode(AstNode.None, Span());
    AstNode.inferred_type = new AstNode(AstNode.InferredType, Span());
}
