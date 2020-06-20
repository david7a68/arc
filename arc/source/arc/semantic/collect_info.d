module arc.semantic.collect_info;

import arc.data.ast;
import arc.data.symbol;
import arc.data.type;
import arc.memory : VirtualMemory;

// void collect_declarations(SymbolTable* symbols, AstNode*[] syntax...) {
//     foreach (node; syntax) switch (node.kind) with (AstNode.Kind) {
//     default:
//         node.symbol_table = symbols.current_scope;
//         collect_declarations(symbols, node.children);
//         break;

//     case Block:
//     case List:
//         // Scope {
//         //     Body
//         // }
//         node.symbol_table = symbols.push_scope();
//         foreach (child; node.children)
//             collect_declarations(symbols, child);
//         symbols.pop_scope();
//         break;

//     case Function:
//         // FunctionScope {
//         //     Parameters   (Symbols)
//         //     ReturnType
//         //     Body
//         // }
//         auto parameters = node.children[0];
//         assert(parameters.kind == AstNode.Kind.List);

//         node.symbol_table = symbols.push_scope();
//         foreach (param; parameters.children) {
//             assert(param.kind == AstNode.Kind.ListMember);
//             auto name = param.children[0];
//             auto type = param.children[1];
//             auto expr = param.children[2];

//             name.symbol = symbols.make_symbol(Symbol.Kind.FunctionParam, name.text);
//             collect_declarations(symbols, type);
//             collect_declarations(symbols, expr);
//         }

//         collect_declarations(symbols, node.children[1 .. $]);
//         symbols.pop_scope();
//         break;

//     case ListMember:
//         // ListMember {
//         //     Name         (Symbol)
//         //     Type
//         //     Value
//         // }
//         if (auto name = node.children[0])
//             name.symbol = symbols.make_symbol(Symbol.Kind.Variable, name.text);

//         collect_declarations(symbols, node.children[1 .. $]);
//         break;

//     case Definition:
//         auto name = node.children[0];
//         name.symbol = symbols.make_symbol(Symbol.Kind.Constant, name.text);
//         collect_declarations(symbols, node.children[1 .. $]);
//         break;

//     case Variable:
//         // Declaration {
//         //     Name         (Symbol)
//         //     Type
//         //     Value
//         // }
//         auto name = node.children[0];
//         name.symbol = symbols.make_symbol(Symbol.Kind.Variable, name.text);
//         collect_declarations(symbols, node.children[1 .. $]);
//         break;
//     }
// }

// void collect_type_info(ArcTypeAllocator* types, AstNode*[] syntax) {
//     foreach (node; syntax) switch (node.kind) with (AstNode.Kind) {
//     default:
//         collect_type_info(types, node.children);
//         break;

//     case Integer:
//         node.type = ArcType.builtin_uint;
//         break;

//     case Char:
//         node.type = ArcType.builtin_char;
//         break;

//     case String:
//         assert(false, "Not Implemented");
//         // node.type = types.make_type(ArcType.Kind.List, 0, 1, strings.get(node.key).length);

//     case List:
//         auto list = types.make_type(ArcType.Kind.List);
//         list.members = types.member_arrays.alloc(node.children.length);

//         foreach (i, member; node.children) {
//             list.members[i].name = member.children[0].text;
//             if (member.children[1].kind != AstNode.Kind.Inferred) {
//                 list.members[i].type = types.from_type_expr(member.children[1]);
//             }
//         }

//         node.type = list;
//         break;

//     case Variable:
//     case Definition:
//     case Function:
//     case FunctionType:
//     case PointerType:
//         assert(false, "Not Implemented");
//     }
// }

// ArcType* from_type_expr(ArcTypeAllocator* types, AstNode* type_expr) {
//     switch (type_expr.kind) with (AstNode.Kind) {
//     case Inferred:
//         return ArcType.inferred;

//     case PointerType:
//         auto type = types.make_type(ArcType.Kind.Pointer);
//         type.base_type = types.from_type_expr(type_expr.children[0]);
//         return type;

//     case Name:
//         type_expr.symbol = type_expr.symbol_table.lookup(type_expr.text);
//         return type_expr.symbol.type;

//     case List:
//     case Access:
//     case Call:
//         return null;

//     default:
//         assert(false);
//     }
// }
