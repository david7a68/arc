module arc.syntax.syntax_allocator;

import arc.data.ast;
import arc.data.symbol;
import arc.memory;

/// The size of each sequence pool, calculated in number of nodes stored. (3 nodes, 8 nodes, etc).
immutable ulong[] ast_size_classes = [
    3, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096
];

final class SyntaxAllocator {
    import std.algorithm : filter;

public:
    this() {
        _memory = VirtualMemory(128.gib);
        _ast_allocator = TreeAllocator!AstNode(&_memory, ast_size_classes);
        _sym_allocator = ObjectPool!Symbol(&_memory);
    }

    AstNode* alloc_ast(Args...)(Args args) {
        return _ast_allocator.alloc(args);
    }

    void free(AstNode*[] nodes...) {
        foreach (node; nodes.filter!(n => !n.is_marker)) {
            if (node.children.length > 2)
                free_seq(node.children);
            else
                free(node.children);

            if (node.is_resolved_symbol)
                _sym_allocator.free(node.symbol);

            _ast_allocator.free(node);
        }
    }

    Symbol* alloc_sym(Args...)(Args args) {
        return _sym_allocator.alloc(args);
    }

    auto get_ast_appender() {
        return _ast_allocator.get_appender();
    }

    AstNode*[] make_seq(AstNode*[] nodes...) {
        auto size_class = 0;
        for (; ast_size_classes[size_class] < nodes.length; size_class++) {
        }

        auto array = _ast_allocator.alloc_array(size_class)[0 .. nodes.length];
        array[] = nodes;
        return array;
    }

    void free_seq(AstNode*[] nodes) {
        _ast_allocator.free(nodes);
    }

private:
    VirtualMemory _memory;
    ObjectPool!Symbol _sym_allocator;
    TreeAllocator!AstNode _ast_allocator;
}
