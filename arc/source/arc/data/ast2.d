module arc.data.ast2;

import arc.data.source: Span, merge_all;
import arc.data.hash: Key;

struct AstNode {
    enum Kind : ubyte {
        None,
        Invalid,
        Inferred,
        ConstantDeclaration,
        TypeDeclaration,
        Variable,
        Name,
        Integer,
        Char,
        List,
        Block,
        Negate,
        Not,
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

    /// The span of text used to derive this syntax node, including all of its
    /// children.
    Span span; // This is currently the largest part of the node, can we shrink it by putting them elsewhere?
    
    /// Discriminant, provides information for how to interpret this AST node
    Kind kind;

    // Make the padding in this struct explicit
    // Note: Since x86 pointers are 48-bits, we could use this padding as a type pointer.
    //       This is also true for AArch64.
    ubyte[7] padding;

    union {
        private AstNode*[] _children;
        private AstNode* _child;

        /// The interned symbol name for this node.
        Key symbol;
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

    this(Kind kind, Span prefix, AstNode*[] parts) in (prefix <= parts[0].span) {
        this(kind, prefix.merge(parts[$ - 1].span));
        _children = parts;
    }

    bool is_valid() {
        return kind != AstNode.Invalid;
    }

    AstNode* as_invalid(Span span) in (num_children == 0) {
        kind = Kind.Invalid;
        this.span = span;
        return &this;
    }

    size_t num_children() {
        switch (kind) with (Kind) {
            case None: case Invalid: case Inferred:
            case Name: case Integer: case Char:
                return 0;
            case Negate: case Not:
                return _children !is null;
            default:
                return _children.length;
        }
    }

    AstNode*[] children() @nogc {
        switch (kind) with (Kind) {
            case None: case Invalid: case Inferred:
            case Name: case Integer: case Char:
                return [];
            case Negate: case Not:
                return (&_child)[0 .. 1]; // JANK
            default:
                return _children;
        }
    }
}

static assert(AstNode.alignof == 8);
static assert(AstNode.sizeof == 32);

alias SequenceBuffer = SizedSequenceBuffer!4096;
// 2 ^ 14, for a total of 131_072 bytes of memory.
// Hopefully we don't need many of these.
alias LargeSequenceBuffer = SizedSequenceBuffer!16384;

struct SizedSequenceBuffer(size_t capacity) {
private:
    AstNode*[capacity] _nodes;
    size_t _count;

public:
    size_t length() { return _count; }

    void add(AstNode* node) in (length < capacity) {
        _nodes[_count] = node;
        _count++;
    }

    AstNode*[] opSlice() { return _nodes[0 .. _count]; }

    void copy(size_t n)(SizedSequenceBuffer!n* buffer) in (n < capacity) {
        _nodes[0 .. n] = (*buffer)[];
    }
}

static assert(SequenceBuffer.alignof == 8);

final class AstNodeAllocator {
    import arc.memory: VirtualAllocator, ObjectPool, gib;
    import std.experimental.allocator: makeArray;

private:
    /// We reserve 128 Gib of memory for the syntax tree
    enum reserved_bytes = 128.gib;

    VirtualAllocator mem;
    ObjectPool!AstNode nodes;
    ObjectPool!SequenceBuffer sequence_buffers;
    ObjectPool!LargeSequenceBuffer large_sequence_buffers;

public:
    this() {
        mem = VirtualAllocator(reserved_bytes);
        nodes = ObjectPool!AstNode(&mem);
        sequence_buffers = ObjectPool!SequenceBuffer(&mem);
        large_sequence_buffers = ObjectPool!LargeSequenceBuffer(&mem);
    }

    AstNode* alloc(Args...)(Args args) {
        auto node = nodes.allocate();
        *node = AstNode(args);
        return node;
    }

    void free(AstNode*[] free_nodes...) {
        foreach (n; free_nodes) {
            if (n.num_children > 0) free(n.children);
            nodes.deallocate(n);
        }
    }

    /// Borrow a sequence buffer to construct lists of elements.
    SequenceBuffer* alloc_sequence_buffer() {
        return sequence_buffers.allocate();
    }

    LargeSequenceBuffer* upgrade_sequence_buffer(SequenceBuffer* old_buffer) {
        auto large = large_sequence_buffers.allocate();
        large.copy(old_buffer);
        return large;
    }

    AstNode*[] alloc_sequence(SequenceBuffer* seq) {
        auto array = cast(AstNode*[]) mem.allocate((AstNode*).sizeof * seq.length);
        array[] = (*seq)[];
        sequence_buffers.deallocate(seq);
        return array;
    }

    AstNode*[] alloc_sequence(LargeSequenceBuffer* seq) {
        auto array = cast(AstNode*[]) mem.allocate((AstNode*).sizeof * seq.length);
        array[] = (*seq)[];
        large_sequence_buffers.deallocate(seq);
        return array;
    }
}
