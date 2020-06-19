module arc.data.ast;

import arc.data.span;
import arc.data.hash : Key;
import arc.data.symbol : Symbol, ScopedSymbolTable;
import arc.util : case_of;

struct AstNode {
    enum Kind : ubyte {
        None,
        Invalid,
        Inferred,
        Import,
        Definition,
        Variable,
        If,
        Return,
        Break,
        Continue,
        Loop,
        Name,
        Integer,
        Char,
        String,
        List,
        ListMember,
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
        StaticAccess,
        Function,
        FunctionType,
        PointerType
    }

    Kind kind;
    uint num_children;
    Span span;

    union {
        struct {
            union {
                Symbol* symbol;
                ScopedSymbolTable* symbol_table;
            }
            union {
                ulong value;
                Key text;
                private AstNode* _child;
                private AstNode** _children;
            }
        }
        private AstNode*[2] _children_2;
    }

    this(Kind kind, Span span) {
        this.kind = kind;
        this.span = span;
    }

    this(Kind kind, Span span, Key text) {
        this(kind, span);
        this.text = text;
    }

    this(Kind kind, Span prefix, AstNode* child)
    in(prefix <= child.span) {
        this(kind, prefix + child.span);
        _child = child;
        num_children = 1;
    }

    this(Kind kind, AstNode* left, AstNode* right, Span prefix = Span()) {
        this(kind, merge_all(prefix, left.span, right.span));
        _children_2 = [left, right];
    }

    this(Kind kind, Span outer, AstNode*[] parts) {
        this(kind, outer);
        _children = parts.ptr;
        num_children = cast(uint) parts.length;
    }

    this(Kind kind, AstNode*[] parts, Span extra = Span()) {
        this(kind, merge_all(extra, parts[0].span, parts[$ - 1].span));
        _children = parts.ptr;
        num_children = cast(uint) parts.length;
    }

    static inferred() {
        return cast(AstNode*)&_inferred;
    }

    static none() {
        return cast(AstNode*)&_none;
    }

    bool is_marker() const {
        return kind == Kind.None || kind == Kind.Inferred;
    }

    bool is_some() const {
        return !is_marker && kind != Kind.Invalid;
    }

    AstNode* as_invalid(Span span) return 
    in(children.length == 0) {
        this = AstNode(Kind.Invalid, span);
        return &this;
    }

    AstNode* respan(Span span) return  {
        this.span = span;
        return &this;
    }

    AstNode*[] children() return  {
        switch (kind) with (Kind) {
        mixin(case_of(None, Invalid, Inferred, Name, Integer, Char, String));
            return [];
        mixin(case_of(Negate, Not, PointerType, Return, Loop, Import));
            return (&_child)[0 .. 1]; // JANK
        case Assign: .. case StaticAccess:
            return _children_2;
        default:
            return _children[0 .. num_children];
        }
    }
}

bool is_valid(AstNode*[] nodes...) {
    foreach (node; nodes)
        if (node.kind == AstNode.Kind.Invalid)
            return false;
    return true;
}

private const _inferred = AstNode(AstNode.Kind.Inferred, Span());
private const _none = AstNode(AstNode.Kind.None, Span());
