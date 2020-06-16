module tests.arc.scope_tree;

import arc.data.ast : AstNode;
import arc.data.source : Span;
import arc.data.symbol : Symbol;
import arc.memory;
import arc.semantic.scope_tree;

@("ScopeBuilder Memory Management") unittest {
    auto memory1 = VirtualMemory(1.gib);
    auto tree_nodes = TreeAllocator!ScopeTreeNode(&memory1);

    auto memory2 = VirtualMemory(1.gib);
    auto symbols = ObjectPool!Symbol(&memory2);

    auto builder = ScopeBuilder(&symbols, &tree_nodes);
    assert(builder.current && builder.current is builder.root);
    assert(builder.current.kind == ScopeTreeNode.Kind.FileScope);

    {
        const symbol1 = builder.add_symbol(Symbol.Kind.Char, 1234);
        assert(symbol1 && symbol1.kind == Symbol.Kind.Char && symbol1.name == 1234);
        const node1 = builder.current.children.get()[0];
        assert(node1.kind == ScopeTreeNode.Kind.Symbol && node1.symbol is symbol1);
    }
    {
        auto ast_nodes = ObjectPool!AstNode(&memory2);
        auto ast1 = ast_nodes.alloc(AstNode.Kind.Name, Span());
        ast1.is_resolved_symbol = true; // to test correct flag setting
        builder.add_unresolved_symbol(ast1);
        assert(!ast1.is_resolved_symbol && ast1.symbol is Symbol.unresolved);
        const node2 = builder.current.children.get()[1];
        assert(node2.kind == ScopeTreeNode.Kind.UnresolvedSymbol && node2.unresolved_node is ast1);
    }
}
