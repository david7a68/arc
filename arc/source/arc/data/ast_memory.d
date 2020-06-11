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
        _allocator.free_array(nodes);
    }

private:
    TreeAllocator!(AstNode, size_classes) _allocator;
}
