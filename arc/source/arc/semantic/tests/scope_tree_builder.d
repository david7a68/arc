module arc.semantic.tests.scope_tree_builder;

import arc.semantic.scope_tree: ScopeTree;
import arc.semantic.symbol: Symbol;
import arc.ast: AstNode;
import arc.data.source: Span;

// ScopeTree get_scope_tree(AstNode* node) {
//     import arc.semantic.scope_tree: ScopeTreeBuilder, collect_symbols;

//     auto builder = ScopeTreeBuilder.initialize();
//     collect_symbols(builder, node);

//     return builder.scope_tree;
// }

// Symbol[] get_symbols(AstNode* node) {
//     return get_scope_tree(node).symbols;
// }

// Symbol[] resolve_references(ScopeTree tree) {
//     import arc.semantic.scope_tree: resolve_symbols;

//     resolve_symbols(tree.symbols, tree.root_scope);

//     return tree.symbols;
// }

// bool check_types(Symbol[] symbols, Symbol.Kind[] expected...) {
//     import std.algorithm: min;
//     import std.range: lockstep;
    
//     // The first symbol is always reserved for invalid values
//     symbols = symbols[];

//     const same_length = symbols.length == expected.length;
//     const min_length = min(symbols.length, expected.length);

//     foreach (ref sym, exp; lockstep(symbols[0 .. min_length], expected[0 .. min_length]))
//         if (sym.kind != exp)
//             return false;

//     if (!same_length && min_length > 0) {
//         return false;
//     }

//     return true;
// }

// // bool check_references(Symbol[] symbols, size_t[] reference_ids...) {
// //     import std.algorithm: min, filter;
// //     import std.range: lockstep;

// //     symbols = symbols[1 .. $];

// //     size_t i = 0;
// //     foreach (ref sym, id; lockstep(symbols.filter!(a => a.kind == Symbol.Reference), reference_ids)) {
// //         if (sym.ref_id != id)
// //             return false;
// //         i++;
// //     }

// //     if (i + 1 == reference_ids.length)
// //         return false;
    
// //     return true;
// // }

// @("build symbol empty scope") unittest {
//     assert(get_symbols(new AstNode(AstNode.Module, Span())).check_types());
// }

// @("build symbol simple variable") unittest {
//     // a := 1
//     assert(
//         get_symbols(
//             new AstNode(AstNode.Define, Span(), [
//                 new AstNode(AstNode.Name, Span(), 0),
//                 AstNode.inferred_type,
//                 new AstNode(AstNode.Integer, Span(), 1)
//             ])
//         ).check_types(
//             Symbol.DefConstant
//         )
//     );
// }

// @("build symbol simple function") unittest {
//     // a := () -> 1
//     assert(
//         get_symbols(
//             new AstNode(AstNode.Define, Span(), [
//                 new AstNode(AstNode.Name, Span(), 0),
//                 AstNode.inferred_type,
//                 new AstNode(AstNode.Function, Span(), [
//                     new AstNode(AstNode.TypeList, Span()),
//                     AstNode.inferred_type,
//                     new AstNode(AstNode.Integer, Span(), 1)
//                 ])
//             ])
//         ).check_types(
//             Symbol.DefFunction
//         )
//     );
// }

// @("build symbol block") unittest {
//     // { a := 0; b := 1; a + b }
//     with (AstNode.Type)
//     assert(
//         get_symbols(
//             new AstNode(Block, Span(), [
//                 new AstNode(Variable, Span(), [
//                     new AstNode(Name, Span(), 1),
//                     AstNode.inferred_type,
//                     new AstNode(Integer, Span())
//                 ]),
//                 new AstNode(Variable, Span(), [
//                     new AstNode(Name, Span(), 2),
//                     AstNode.inferred_type,
//                     new AstNode(Integer, Span())
//                 ]),
//                 new AstNode(Add, Span(), [
//                     new AstNode(Name, Span(), 1),
//                     new AstNode(Name, Span(), 2)
//                 ])
//             ])
//         ).check_types(
//             Symbol.NewVariable,
//             Symbol.NewVariable,
//             Symbol.Reference,
//             Symbol.Reference
//         )
//     );
// }

// @("build symbol type") unittest {
//     // def T : int
//     with (AstNode.Type)
//     assert(
//         get_symbols(
//             new AstNode(Define, Span(), [
//                 new AstNode(Name, Span(), 1),
//                 new AstNode(Name, Span(), 2),
//                 AstNode.none
//             ])
//         ).check_types(
//             Symbol.DefType,
//             Symbol.Reference
//         )
//     );
// }

// @("build extracted symbol") unittest {
//     // if a { b := c }
//     with (AstNode.Type)
//     assert(
//         get_symbols(
//             new AstNode(If, Span(), [
//                 new AstNode(Name, Span(), 1),
//                 new AstNode(Block, Span(), [
//                     new AstNode(Variable, Span(), [
//                         new AstNode(Name, Span(), 2),
//                         AstNode.inferred_type,
//                         new AstNode(Name, Span(), 3)
//                     ])
//                 ]),
//                 AstNode.none
//             ])
//         ).check_types(
//             Symbol.Reference,
//             Symbol.NewVariable,
//             Symbol.Reference
//         )
//     );
// }

// @("build call symbol") unittest {
//     with (AstNode.Type)
//     assert(
//         get_symbols(
//             new AstNode(Call, Span(), [
//                 new AstNode(Name, Span(), 1),
//                 new AstNode(List, Span(), [
//                     new AstNode(ListMember, Span(), [
//                         AstNode.none,
//                         AstNode.inferred_type,
//                         new AstNode(Name, Span(), 2)
//                     ])
//                 ])
//             ])
//         ).check_types(
//             Symbol.Reference,
//             Symbol.Reference
//         )
//     );
// }

// // @("resolve symbol") unittest {
// //     // a := 2; { a }
// //     with (AstNode.Type)
// //     assert(
// //         get_scope_tree(
// //             new AstNode(Module, Span(), [
// //                 new AstNode(Variable, Span(), [
// //                     new AstNode(Name, Span(), 1),
// //                     AstNode.inferred_type,
// //                     new AstNode(Integer, Span(), 2)
// //                 ]),
// //                 new AstNode(Block, Span(), [
// //                     new AstNode(Name, Span(), 1)
// //                 ])
// //             ])
// //         ).resolve_references().check_references(
// //             1
// //         )
// //     );
// // }

// // @("failed resolve symbol") unittest {
// //     // { a := 2 } a
// //     with (AstNode.Type)
// //     assert(
// //         get_scope_tree(
// //             new AstNode(Module, Span(), [
// //                 new AstNode(Block, Span(), [
// //                     new AstNode(Variable, Span(), [
// //                         new AstNode(Name, Span(), 1),
// //                         AstNode.inferred_type,
// //                         new AstNode(Integer, Span(), 2)
// //                     ]),
// //                 ]),
// //                 new AstNode(Name, Span(), 1)
// //             ])
// //         ).resolve_references().check_references(
// //             0
// //         )
// //     );
// // }
