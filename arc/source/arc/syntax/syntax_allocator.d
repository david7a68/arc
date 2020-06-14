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
    this() {
        _memory = VirtualMemory(128.gib);
        _ast_nodes = ObjectPool!AstNode(&_memory);
        _ast_arrays = ArrayPool!(AstNode*)(&_memory, ast_size_classes);
    }

    AstNode* alloc_ast(Args...)(Args args) {
        return _ast_nodes.alloc(args);
    }

    void free(AstNode*[] nodes...) {
        foreach (node; nodes.filter!(n => !n.is_marker)) {
            if (node.children.length > 2)
                free_seq(node.children);
            else
                free(node.children);

            if (node.is_resolved_symbol)
                _symbols.free(node.symbol);

            _ast_nodes.free(node);
        }
    }

    auto get_ast_appender() {
        return _ast_arrays.get_appender();
    }

    AstNode*[] make_seq(AstNode*[] nodes...) {
        auto size_class = 0;
        for (; ast_size_classes[size_class] < nodes.length; size_class++) {
        }

        auto array = _ast_arrays.alloc_size_class(size_class)[0 .. nodes.length];
        array[] = nodes;
        return array;
    }

    void free_seq(AstNode*[] nodes) {
        free(nodes);
        _ast_arrays.free(nodes);
    }

private:
    VirtualMemory _memory;
    ObjectPool!AstNode _ast_nodes;
    ArrayPool!(AstNode*) _ast_arrays;
}
