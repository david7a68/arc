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

    Span span;
    Kind kind;

    // TODO: Make use of unused top 16 bits on x64 (and Aarch64) to store
    //       discriminant, and turn remaining bits to type pointer.
    private ubyte[7] _type_ptr;

    union {
        private AstNode*[] _children;
        private AstNode* _child;
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

    this(Kind kind, Span outer, AstNode*[] parts) {
        this(kind, outer);
        _children = parts;
    }

    this(Kind kind, AstNode*[] parts) {
        this(kind, parts[0].span.merge(parts[$ - 1].span));
        _children = parts;
    }

    bool is_valid() { return kind != Kind.Invalid; }

    AstNode* as_invalid(Span span) in (children.length == 0) {
        this = AstNode(Kind.Invalid, span);
        return &this;
    }

    AstNode*[] children() {
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

alias SequenceBuffer = SizedSequenceBuffer!4096; // 128 kib
alias LargeSequenceBuffer = SizedSequenceBuffer!16384; // 512 kib

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

static assert(AstNode.alignof == 8 && AstNode.sizeof == 32);
static assert(SequenceBuffer.alignof == 8 && LargeSequenceBuffer.alignof == 8);

final class AstNodeAllocator {
    import arc.memory: VirtualAllocator, ObjectPool, gib;

private:
    /// We reserve 128 Gib of memory for the syntax tree.
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
            if (n.children.length > 0) free(n.children);
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
        auto array = alloc_sequence(seq.opSlice());
        sequence_buffers.deallocate(seq);
        return array;
    }

    AstNode*[] alloc_sequence(LargeSequenceBuffer* seq) {
        auto array = alloc_sequence(seq.opSlice());
        large_sequence_buffers.deallocate(seq);
        return array;
    }

    AstNode*[] alloc_sequence(AstNode*[] seq...) {
        auto array = cast(AstNode*[]) mem.allocate((AstNode*).sizeof * seq.length);
        array[] = seq[];
        return array;
    }
}
