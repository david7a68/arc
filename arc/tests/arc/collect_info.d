module tests.arc.collect_info;

// import arc.data.ast;
// import arc.data.span;
// import arc.data.symbol;
// import arc.memory;
// import arc.semantic.collect_info;
// import arc.syntax.syntax_allocator;

// @("Scope Trees from Variables") unittest {
//     auto vm = VirtualMemory(4.kib);
//     auto syntax = new SyntaxAllocator(&vm);
//     auto symbols = SymbolTable(&vm);

//     // dfmt off
//     // a := b + c;
//     with (AstNode.Kind) {
//         auto ast = syntax.alloc_ast(
//             Variable, Span(), syntax.make_seq(
//                 syntax.alloc_ast(Name, Span(), 0),
//                 AstNode.inferred,
//                 syntax.alloc_ast(Add, syntax.make_seq(
//                     syntax.alloc_ast(Name, Span(), 0),
//                     syntax.alloc_ast(Name, Span(), 0)))));
//         collect_declarations(&symbols, ast);

//         assert(symbols.num_symbols == 1);

//         assert(ast.children[0].symbol.kind == Symbol.Kind.Variable);

//         const add_lhs = ast.children[2].children[0];
//         assert(add_lhs.kind == AstNode.Kind.Name && add_lhs.symbol is null);

//         const add_rhs = ast.children[2].children[1];
//         assert(add_rhs.kind == AstNode.Kind.Name && add_rhs.symbol is null);
//     }
//     // dfmt on
// }

// @("Scope Trees from Blocks") unittest {
//     auto vm = VirtualMemory(4.kib);
//     auto syntax = new SyntaxAllocator(&vm);
//     auto symbols = SymbolTable(&vm);

//     // dfmt off
//     with (AstNode.Kind) {
//         auto ast = syntax.alloc_ast(
//             Block, syntax.make_seq(
//                 syntax.alloc_ast(Block, Span()),
//                 syntax.alloc_ast(Block, Span()),
//                 syntax.alloc_ast(Block, Span()),
//                 syntax.alloc_ast(Block, syntax.make_seq(
//                     syntax.alloc_ast(Block, Span()),
//                     syntax.alloc_ast(Block, Span()),
//                     syntax.alloc_ast(Block, Span()),
//                     syntax.alloc_ast(Block, Span()))),
//                 syntax.alloc_ast(Block, Span()),
//                 syntax.alloc_ast(Block, Span())));

//         collect_declarations(&symbols, ast);
//         assert(symbols.num_scopes == 11);
//         assert(symbols.num_symbols == 0);
//     }
//     // dfmt on
// }

// @("Scope Trees from Functions") unittest {
//     auto vm = VirtualMemory(4.kib);
//     auto syntax = new SyntaxAllocator(&vm);
//     auto symbols = SymbolTable(&vm);

//     // dfmt off
//     // (a : T) -> return a
//     with (AstNode.Kind) {
//         auto ast = syntax.alloc_ast(
//             Function, syntax.make_seq(
//                 syntax.alloc_ast(List, syntax.make_seq(
//                     syntax.alloc_ast(ListMember, syntax.make_seq(
//                         syntax.alloc_ast(Name, Span(), 0),
//                         syntax.alloc_ast(Name, Span(), 0),
//                         AstNode.inferred)))),
//                     AstNode.inferred,
//                     syntax.alloc_ast(Return, Span(),
//                         syntax.alloc_ast(Name, Span(), 0))));

//         collect_declarations(&symbols, ast);
//         assert(symbols.num_scopes == 1);
//         assert(symbols.num_symbols == 1);

//         auto params = ast.children[0];
//         auto param0 = params.children[0];
//         assert(param0.children[0].kind == AstNode.Kind.Name);
//         assert(param0.children[0].symbol && param0.children[0].symbol.kind == Symbol.Kind.FunctionParam);
//     }
//     // dfmt on
// }
