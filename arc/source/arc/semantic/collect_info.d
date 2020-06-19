module arc.semantic.collect_info;

import arc.data.ast;
import arc.data.symbol;
import arc.memory : VirtualMemory;

void collect_semantic_info(SymbolTable* symbols, AstNode*[] syntax...) {
    foreach (node; syntax) switch (node.kind) with (AstNode.Kind) {
    default:
        collect_semantic_info(symbols, node.children);
        break;

    case Block:
    case List:
        // Scope {
        //     Body
        // }
        symbols.push_scope();
        foreach (child; node.children)
            collect_semantic_info(symbols, child);
        symbols.pop_scope();
        break;

    case Function:
        // FunctionScope {
        //     Parameters   (Symbols)
        //     ReturnType
        //     Body
        // }
        auto parameters = node.children[0];
        assert(parameters.kind == AstNode.Kind.List);

        symbols.push_scope();
        foreach (param; parameters.children) {
            assert(param.kind == AstNode.Kind.ListMember);
            auto name = param.children[0];
            auto type = param.children[1];
            auto expr = param.children[2];

            make_decl_symbol(symbols, Symbol.Kind.FunctionParam, name);

            collect_semantic_info(symbols, type);
            collect_semantic_info(symbols, expr);
        }

        collect_semantic_info(symbols, node.children[1 .. $]);
        symbols.pop_scope();
        break;

    case ListMember:
        // ListMember {
        //     Name         (Symbol)
        //     Type
        //     Value
        // }
        if (node.children[0].is_some)
            make_decl_symbol(symbols, Symbol.Kind.FunctionParam, node.children[0]);

        collect_semantic_info(symbols, node.children[1 .. $]);
        break;

    case Definition:
        make_decl_symbol(symbols, Symbol.Kind.Constant, node.children[0]);
        collect_semantic_info(symbols, node.children[1 .. $]);
        break;

    case Variable:
        // Declaration {
        //     Name         (Symbol)
        //     Type
        //     Value
        // }
        make_decl_symbol(symbols, Symbol.Kind.Variable, node.children[0]);
        collect_semantic_info(symbols, node.children[1 .. $]);
        break;
    }
}

void make_decl_symbol(SymbolTable* symbols, Symbol.Kind kind, AstNode* name)
in(name.kind == AstNode.Kind.Name) {
    auto symbol = symbols.make_symbol(kind, name.text);
    name.symbol = symbol;
    name.is_resolved_symbol = true;
}
