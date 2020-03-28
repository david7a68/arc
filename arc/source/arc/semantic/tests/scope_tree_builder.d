module arc.semantic.tests.scope_tree_builder;

import arc.semantic.symbol: Symbol;
import arc.syntax.ast: AstNode;
import arc.source: Span;

Symbol[] get_symbols(AstNode* node) {
    import arc.semantic.scope_tree: ScopeTree, ScopeTreeBuilder, collect_declarations;

    auto builder = ScopeTreeBuilder.initialize();
    collect_declarations(builder, node);

    return builder.scope_tree.symbols.dup;
}

bool check_types(Symbol[] symbols, Symbol.Kind[] expected...) {
    import std.algorithm: min;
    import std.range: lockstep;
    
    const same_length = symbols.length == expected.length;
    const min_length = min(symbols.length, expected.length);

    foreach (ref sym, exp; lockstep(symbols[0 .. min_length], expected[0 .. min_length]))
        if (sym.kind != exp)
            return false;

    if (!same_length && min_length > 0) {
        return false;
    }

    return true;
}

@("build symbol empty scope") unittest {
    assert(get_symbols(new AstNode(AstNode.Module, Span())).check_types());
}

@("build symbol simple variable") unittest {
    // a := 1
    assert(
        get_symbols(
            new AstNode(AstNode.Define, Span(), [
                new AstNode(AstNode.Name, Span(), 0),
                AstNode.inferred_type,
                new AstNode(AstNode.Integer, Span(), 1)
            ])
        ).check_types(
            Symbol.DefConstant
        )
    );
}

@("build symbol simple function") unittest {
    // a := () -> 1
    assert(
        get_symbols(
            new AstNode(AstNode.Define, Span(), [
                new AstNode(AstNode.Name, Span(), 0),
                AstNode.inferred_type,
                new AstNode(AstNode.Function, Span(), [
                    new AstNode(AstNode.TypeList, Span()),
                    AstNode.inferred_type,
                    new AstNode(AstNode.Integer, Span(), 1)
                ])
            ])
        ).check_types(
            Symbol.DefFunction
        )
    );
}

@("build symbol block") unittest {
    // { a := 0; b := 1; a + b }
    with (AstNode.Type)
    assert(
        get_symbols(
            new AstNode(Block, Span(), [
                new AstNode(Variable, Span(), [
                    new AstNode(Name, Span()),
                    AstNode.inferred_type,
                    new AstNode(Integer, Span())
                ]),
                new AstNode(Variable, Span(), [
                    new AstNode(Name, Span()),
                    AstNode.inferred_type,
                    new AstNode(Integer, Span())
                ]),
                new AstNode(Add, Span(), [
                    new AstNode(Name, Span()),
                    new AstNode(Name, Span())
                ])
            ])
        ).check_types(
            Symbol.NewVariable,
            Symbol.NewVariable
        )
    );
}

@("build symbol type") unittest {
    // def T : int
    with (AstNode.Type)
    assert(
        get_symbols(
            new AstNode(Define, Span(), [
                new AstNode(Name, Span()),
                new AstNode(Name, Span()),
                AstNode.none
            ])
        ).check_types(
            Symbol.DefType
        )
    );
}
