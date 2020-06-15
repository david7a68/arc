module arc.syntax.syntax_allocator;

import arc.data.ast;
import arc.memory;

/// The size of each sequence pool, calculated in number of nodes stored.
/// The numbers are untuned and so for the moment just double.
immutable size_t[] ast_size_classes = [
    // Specially for declarations
    3,
    // All others x2 from 8
    8, 16, 32, 64, 128, 256, 512, 2048, 4096, 8192, 16_384, 32_768, 65_536,
    131_072, 262_144
];

final class SyntaxAllocator {
    import std.algorithm : filter;

public:
    this(VirtualMemory* memory) {
        _allocator = TreeAllocator!AstNode(memory, ast_size_classes);
    }

    AstNode* alloc_ast(Args...)(Args args) {
        return _allocator.objects.alloc(args);
    }

    void free(AstNode*[] nodes...) {
        foreach (node; nodes.filter!(n => !n.is_marker)) {
            if (node.children.length > 2)
                free_seq(node.children);
            else
                free(node.children);

            _allocator.objects.free(node);
        }
    }

    auto get_ast_appender() {
        return _allocator.arrays.get_appender();
    }

    AstNode*[] make_seq(AstNode*[] nodes...) {
        auto array = _allocator.arrays.alloc(nodes.length);
        array[] = nodes;
        return array;
    }

    void free_seq(AstNode*[] nodes) {
        free(nodes);
        _allocator.arrays.free(nodes);
    }

private:
    TreeAllocator!AstNode _allocator;
}
