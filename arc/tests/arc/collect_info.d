module tests.arc.collect_info;

import arc.data.ast;
import arc.data.source;
import arc.data.symbol;
import arc.memory;
import arc.semantic.collect_info;
import arc.semantic.scope_tree;
import arc.syntax.syntax_allocator;

struct TestData {
    string test_name;
    ScopeTreeNode* top_scope;
}

TestData get_semantic_info(VirtualMemory* mem, string name, AstNode*[] ast...) {
    auto sym = ObjectPool!Symbol(mem);
    auto scp = TreeAllocator!ScopeTreeNode(mem);
    auto bld = ScopeBuilder(&sym, &scp);

    collect_semantic_info(&bld, ast);
    return TestData(name, bld.root);
}

bool is_symbol_equivalent(TestData data, Symbol.Kind[] expected_symbols...) {
    import std.algorithm : equal;
    import std.stdio : writefln;

    Symbol.Kind[] flattened;
    void flatten(ScopeTreeNode* s)
    in(s) {
        if (s.kind == ScopeTreeNode.Kind.Symbol) {
            flattened ~= s.symbol.kind;
        }
        else if (s.kind == ScopeTreeNode.Kind.UnresolvedSymbol) {
            flattened ~= Symbol.Kind.Unresolved;
        }
        else {
            foreach (child; s.children.get())
                flatten(child);
        }
    }

    flatten(data.top_scope);
    const are_equal = equal(flattened, expected_symbols);

    if (!are_equal) {
        writefln("Test failed: The scope tree does not match the expected types.\n"
                ~ "Test Name: %s\nTree:\n\t%s\nExpected:\n\t%s",
                data.test_name, flattened, expected_symbols);
    }

    return are_equal;
}

bool check_symbols(TestData data, Symbol.Kind[] expected_symbols...) {
    return is_symbol_equivalent(data, expected_symbols);
}

@("Scope Trees from Variables") unittest {
    auto vm = VirtualMemory(4.kib);
    auto syntax = new SyntaxAllocator(&vm);

    // dfmt off
    // a := b + c;
    with (AstNode.Kind) {
        auto ast = syntax.alloc_ast(
            Variable, Span(), syntax.make_seq(
                syntax.alloc_ast(Name, Span(), 0),
                AstNode.inferred,
                syntax.alloc_ast(Add,
                    syntax.alloc_ast(Name, Span(), 0),
                    syntax.alloc_ast(Name, Span(), 0))));

        with (Symbol.Kind)
            assert(get_semantic_info(&vm, "Variable with Value", ast)
                    .check_symbols(Variable, Unresolved, Unresolved));
    }
    // dfmt on
}

@("Scope Trees from Blocks") unittest {
    auto vm = VirtualMemory(4.kib);
    auto syntax = new SyntaxAllocator(&vm);

    // dfmt off
    with (AstNode.Kind) {
        auto ast = syntax.alloc_ast(
            Block, syntax.make_seq(
                syntax.alloc_ast(Block, Span()),
                syntax.alloc_ast(Block, Span()),
                syntax.alloc_ast(Block, Span()),
                syntax.alloc_ast(Block, syntax.make_seq(
                    syntax.alloc_ast(Block, Span()),
                    syntax.alloc_ast(Block, Span()),
                    syntax.alloc_ast(Block, Span()),
                    syntax.alloc_ast(Block, Span()))),
                syntax.alloc_ast(Block, Span()),
                syntax.alloc_ast(Block, Span())));
        // dfmt on

        assert(get_semantic_info(&vm, "Scopes Trees from Blocks", ast).check_symbols());
    }
}

@("Scope Trees from Functions") unittest {
    auto vm = VirtualMemory(4.kib);
    auto syntax = new SyntaxAllocator(&vm);

    // dfmt off
    // (a : T) -> return a
    with (AstNode.Kind) {
        auto ast = syntax.alloc_ast(
            Function, syntax.make_seq(
                syntax.alloc_ast(List, syntax.make_seq(
                    syntax.alloc_ast(ListMember, syntax.make_seq(
                        syntax.alloc_ast(Name, Span(), 0),
                        syntax.alloc_ast(Name, Span(), 0),
                        AstNode.inferred)))),
                    AstNode.inferred,
                    syntax.alloc_ast(Return, Span(),
                        syntax.alloc_ast(Name, Span(), 0))));

        assert(get_semantic_info(&vm, "Scope Trees from Functions", ast).check_symbols(
                Symbol.Kind.FunctionParam, Symbol.Kind.Unresolved,
                Symbol.Kind.Unresolved));
    }
    // dfmt on
}
