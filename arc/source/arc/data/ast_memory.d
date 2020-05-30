/**
 The `ast_memory` module contains the AST node allocator as well as facilities
 for constructing arrays of nodes, termed sequences in this module.

 In general, sequences should be constructed with the SequenceBuilder struct,
 because it will automatically resize the sequence as necessary. Once
 construction is complete, simply call `builder.nodes()` to extract the sequence
 as an array, and go on your way.

 The allocator exposes only the mechanism for allocating and freeing nodes or
 arrays of nodes. HOWEVER you should never directly free a sequence. Only free a
 sequence as a node's children, otherwise the sequence itself will not be freed.
 */
module arc.data.ast_memory;

import arc.data.ast;

/// The size of each sequence pool, calculated in number of nodes stored. (3 nodes, 8 nodes, etc).
immutable size_classes = [3, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096];

struct SequenceBuilder {
private:
    AstNodeAllocator _mem;
    Sequence* _buffer;

public:
    this(AstNodeAllocator allocator) { _mem = allocator; _buffer = _mem.alloc_seq(0); }

    @disable this(this);

    AstNode*[] nodes() { return _buffer.nodes.ptr[0 .. _buffer.count]; }

    void abort() { _mem.free_seq(_buffer); }

    void add(AstNode* node) {
        const current_capacity = size_classes[_buffer.size_class];

        if (_buffer.count == current_capacity)
            _buffer = _mem.alloc_seq(_buffer.size_class + 1, _buffer);

        _buffer.nodes.ptr[_buffer.count] = node;
        _buffer.count++;
    }
}

final class AstNodeAllocator {
    import std.algorithm: filter;
    import core.stdc.string: memcpy;
    import arc.memory: MemoryPool, ObjectPool, VirtualAllocator, gib;

    enum virtual_size = 128.gib;

private:
    VirtualAllocator _memory;
    ObjectPool!AstNode _nodes;
    MemoryPool[size_classes.length] _seq_pools;

public:
    this() {
        _memory = VirtualAllocator(virtual_size);
        _nodes = ObjectPool!AstNode(&_memory);

        foreach (i, ref pool; _seq_pools)
            pool = MemoryPool(&_memory, footprint_of(i));
    }

    AstNode* alloc(Args...)(Args args) { return _nodes.alloc(args); }

    AstNode*[] make_seq(AstNode*[] nodes...) {
        auto min_class = 0;
        for (; size_classes[min_class] < nodes.length; min_class++) {}

        auto seq = alloc_seq_from_nodes(min_class, nodes);
        return seq.nodes.ptr[0 .. nodes.length];
    }

    void free(AstNode*[] nodes...) {
        foreach (node; nodes.filter!(n => !n.is_marker)) {
            if (node.children) {
                free(node.children);

                if (node.children.length > 2) free_seq(seq_of(node.children));
            }

            _nodes.free(node);
        }
    }

private:
    Sequence* alloc_seq(size_t size_class, Sequence* old = null) in (size_class < size_classes.length) {
        auto seq = alloc_seq_from_nodes(size_class, old ? old.nodes.ptr[0 .. old.count] : null);
        if (old) free_seq(old);
        return seq;
    }

    Sequence* alloc_seq_from_nodes(size_t size_class, AstNode*[] nodes = null) in (size_class < size_classes.length) {
        import std.conv: emplace;

        auto mem = cast(Sequence*) _seq_pools[size_class].alloc().ptr;
        auto seq = mem.emplace!Sequence(cast(ushort) size_class);

        if (nodes) {
            memcpy(seq.nodes.ptr, nodes.ptr, nodes.length * (AstNode*).sizeof);
            seq.count = cast(ushort) nodes.length;
        }

        return seq;
    }

    void free_seq(Sequence* seq) in (seq !is null) {
        _seq_pools[seq.size_class].free(memory_of(seq));
    }
}

private:

auto footprint_of(size_t size_class) {
    return Sequence.sizeof + ((AstNode*).sizeof * size_classes[size_class]);
}

auto memory_of(Sequence* seq) {
    return (cast(void*) seq)[0 .. footprint_of(seq.size_class)];
}

auto seq_of(AstNode*[] nodes) {
    return cast(Sequence*) ((cast(void*) nodes.ptr) - Sequence.sizeof);
}

static assert(Sequence.alignof == AstNode.alignof);
static assert(Sequence.alignof == (void*).alignof);

struct Sequence {
    ushort size_class, count;
    ubyte[4] _padding;
    AstNode*[0] nodes;
}

@("hat")
unittest {
    import arc.data.source: Span;

    auto mem = new AstNodeAllocator();
    auto seq = SequenceBuilder(mem);
    assert(seq._buffer is seq_of(seq.nodes));

    AstNode* node;
    {
        foreach (i; 0 .. 4000)
            seq.add(mem.alloc(AstNode.Invalid, Span(i, 0)));

        auto nodes = seq.nodes;
        foreach (i; 0 .. 4000)
            assert(nodes[i].span.start == i);

        node = mem.alloc(AstNode.Block, seq.nodes);
    }
    mem.free(node);
    assert(mem.alloc_seq(10) is seq._buffer);
}
