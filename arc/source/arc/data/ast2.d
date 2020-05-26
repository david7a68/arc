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

    AstNode* as_invalid(Span span) return in (children.length == 0) {
        this = AstNode(Kind.Invalid, span);
        return &this;
    }

    AstNode*[] children() return {
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

struct SequenceBuffer {
private:
    AstNode*[] _nodes;
    size_t _count, _size_class;

    this(AstNode*[] nodes, size_t size_index) { _nodes = nodes; _size_class = size_index; }

public:
    size_t length()     { return _count; }
    size_t capacity()   { return _nodes.length; }
    size_t size_class() { return _size_class; }

    void add(AstNode* node) in (length < capacity) {
        _nodes[_count] = node;
        _count++;
    }

    AstNode*[] opSlice() { return _nodes[0 .. _count]; }

    void copy(SequenceBuffer* buffer) in (buffer.capacity < capacity) {
        _nodes[0 .. buffer.length] = (*buffer)[];
    }
}

static assert(AstNode.alignof == 8 && AstNode.sizeof == 32);
static assert(SequenceBuffer.alignof == 8);

/// The size classes for sequence buffers, quadrupling each step up.
immutable sequence_pool_sizes = [64, 256, 1024, 4096, 16384];

final class AstNodeAllocator {
    import arc.memory: VirtualAllocator, MemoryPool, ObjectPool, gib;

private:
    /// We reserve 128 Gib of memory for the syntax tree.
    enum reserved_bytes = 128.gib;

    VirtualAllocator mem;
    ObjectPool!AstNode nodes;
    MemoryPool[sequence_pool_sizes.length] sequence_pools;

public:
    this() {
        mem = VirtualAllocator(reserved_bytes);
        nodes = ObjectPool!AstNode(&mem);

        foreach (i, size; sequence_pool_sizes)
            sequence_pools[i] = MemoryPool(&mem, size);
    }

    AstNode* alloc(Args...)(Args args) { return nodes.alloc(args); }

    void free(AstNode*[] free_nodes...) {
        foreach (AstNode* n; free_nodes) {
            if (n.children.length > 0) free(n.children);
            nodes.free(n);
        }
    }

    SequenceBuffer alloc_sequence_buffer() {
        return SequenceBuffer(cast(AstNode*[]) sequence_pools[0].alloc(), 0);
    }

    SequenceBuffer upgrade_sequence_buffer(SequenceBuffer old) {
        const new_size_class = old.size_class + 1;
        assert(new_size_class < sequence_pools.length);

        auto large = SequenceBuffer(cast(AstNode*[]) sequence_pools[new_size_class].alloc(), new_size_class);
        large.copy(&old);
        return large;
    }

    AstNode*[] alloc_sequence(SequenceBuffer seq) {
        auto array = alloc_sequence(seq[]);
        sequence_pools[seq.size_class].free(cast(void[]) seq._nodes);
        return array;
    }

    AstNode*[] alloc_sequence(AstNode*[] seq...) {
        auto array = cast(AstNode*[]) mem.alloc((AstNode*).sizeof * seq.length);
        array[] = seq[];
        return array;
    }
}
