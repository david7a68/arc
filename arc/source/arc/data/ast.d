module arc.data.ast;

import arc.data.source: Span, merge_all;
import arc.data.hash: Key;

struct AstNode {
    enum Kind : ubyte {
        None,
        Invalid,
        Inferred,
        Definition,
        Variable,
        Name,
        Integer,
        Char,
        List,
        Block,
        Negate,
        Not,
        Assign,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,
        Equal,
        NotEqual,
        And,
        Or,
        Call,
        Access,
        Function,
        FunctionType,
    }

    alias Kind this;

    Span span;
    Kind kind;

    // TODO: Make use of unused top 16 bits on x64 (and Aarch64) to store
    //       discriminant, and turn remaining bits to type pointer.
    private ubyte[7] _type_ptr;

    union {
        private AstNode*[] _children;
        private AstNode* _child;
        private AstNode*[2] _children_2;
        Key symbol;
        ulong value;
    }

    this(Kind kind, Span span) {
        this.kind = kind;
        this.span = span;
    }

    this(Kind kind, Span span, Key symbol) {
        this(kind, span);
        this.symbol = symbol;
    }

    this(Kind kind, Span prefix, AstNode* child) in (prefix <= child.span) {
        this(kind, prefix.merge(child.span));
        _child = child;
    }

    this(Kind kind, AstNode* left, AstNode* right, Span prefix = Span()) {
        this(kind, merge_all(prefix, left.span, right.span));
        _children_2 = [left, right];
    }

    this(Kind kind, Span outer, AstNode*[] parts) {
        this(kind, outer);
        _children = parts;
    }

    this(Kind kind, AstNode*[] parts) {
        this(kind, parts[0].span.merge(parts[$ - 1].span));
        _children = parts;
    }

    static inferred()   { return cast(AstNode*) &_inferred; }
    static none()       { return cast(AstNode*) &_none; }

    bool is_marker() const { return kind == Kind.None || kind == Kind.Inferred; }

    bool is_valid() const { return kind != Kind.Invalid; }

    AstNode* as_invalid(Span span) return in (children.length == 0) {
        this = AstNode(Kind.Invalid, span);
        return &this;
    }

    AstNode* respan(Span span) return {
        this.span = span;
        return &this;
    }

    AstNode*[] children() return {
        switch (kind) with (Kind) {
            case None: case Invalid: case Inferred:
            case Name: case Integer: case Char:
                return [];
            case Negate: case Not:
                return (&_child)[0 .. 1]; // JANK
            case Assign: .. case Access:
                return _children_2;
            default:
                return _children;
        }
    }
}

private const _inferred = AstNode(AstNode.Inferred, Span());
private const _none = AstNode(AstNode.None, Span());
