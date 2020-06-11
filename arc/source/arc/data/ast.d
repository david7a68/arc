module arc.data.ast;

import arc.data.source: Span, merge_all;
import arc.data.hash: Key;
import arc.util: case_of;

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

    this(Kind kind, AstNode*[] parts, Span extra = Span()) {
        this(kind, merge_all(extra, parts[0].span, parts[$ - 1].span));
        _children = parts;
    }

    static inferred()   { return cast(AstNode*) &_inferred; }
    static none()       { return cast(AstNode*) &_none; }

    bool is_marker() const { return kind == Kind.None || kind == Kind.Inferred; }

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
            mixin(case_of(None, Invalid, Inferred, Name, Integer, Char, String));
                return [];
            mixin(case_of(Negate, Not, PointerType, Return, Loop, Import));
                return (&_child)[0 .. 1]; // JANK
            case Assign: .. case StaticAccess:
                return _children_2;
            default:
                return _children;
        }
    }
}

bool is_valid(AstNode*[] nodes...) {
    foreach (node; nodes) if (node.kind == AstNode.Kind.Invalid) return false;
    return true;
}

private const _inferred = AstNode(AstNode.Kind.Inferred, Span());
private const _none = AstNode(AstNode.Kind.None, Span());

/// The size of each sequence pool, calculated in number of nodes stored. (3 nodes, 8 nodes, etc).
immutable size_classes = [3, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096];

final class AstNodeAllocator {
    import std.algorithm: filter;
    import arc.memory: gib, TreeAllocator;

public:
    this() {
        _allocator = TreeAllocator!(AstNode, size_classes)(uint.max + 1);
    }

    AstNode* alloc(Args...)(Args args) { return _allocator.alloc(args); }

    void free(AstNode*[] nodes...) {
        foreach (node; nodes.filter!(n => !n.is_marker)) {
            if (node.children) {
                free(node.children);

                if (node.children.length > 2) free_seq(node.children);
            }

            _allocator.free(node);
        }
    }

    auto get_appender() {
        return _allocator.get_appender();
    }

    AstNode*[] make_seq(AstNode*[] nodes...) {
        auto size_class = 0;
        for (; size_classes[size_class] < nodes.length; size_class++) {}

        auto array = _allocator.alloc_array(size_class)[0 .. nodes.length];
        array[] = nodes;
        return array;
    }

    void free_seq(AstNode*[] nodes) {
        _allocator.free(nodes);
    }

private:
    TreeAllocator!(AstNode, size_classes) _allocator;
}
