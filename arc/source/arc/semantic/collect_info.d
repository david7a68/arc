module arc.semantic.collect_info;

import arc.data.ast;
import arc.data.symbol;
import arc.memory : VirtualMemory;

void collect_declarations(SymbolTable* symbols, AstNode*[] syntax...) {
    foreach (node; syntax) switch (node.kind) with (AstNode.Kind) {
    default:
        collect_declarations(symbols, node.children);
        break;

    case Block:
    case List:
        // Scope {
        //     Body
        // }
        node.symbol_table = symbols.push_scope();
        foreach (child; node.children)
            collect_declarations(symbols, child);
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

        node.symbol_table = symbols.push_scope();
        foreach (param; parameters.children) {
            assert(param.kind == AstNode.Kind.ListMember);
            auto name = param.children[0];
            auto type = param.children[1];
            auto expr = param.children[2];

            name.symbol = symbols.make_symbol(Symbol.Kind.FunctionParam, name.text);
            collect_declarations(symbols, type);
            collect_declarations(symbols, expr);
        }

        collect_declarations(symbols, node.children[1 .. $]);
        symbols.pop_scope();
        break;

    case ListMember:
        // ListMember {
        //     Name         (Symbol)
        //     Type
        //     Value
        // }
        if (auto name = node.children[0])
            name.symbol = symbols.make_symbol(Symbol.Kind.Variable, name.text);

        collect_declarations(symbols, node.children[1 .. $]);
        break;

    case Definition:
        auto name = node.children[0];
        name.symbol = symbols.make_symbol(Symbol.Kind.Constant, name.text);
        collect_declarations(symbols, node.children[1 .. $]);
        break;

    case Variable:
        // Declaration {
        //     Name         (Symbol)
        //     Type
        //     Value
        // }
        auto name = node.children[0];
        name.symbol = symbols.make_symbol(Symbol.Kind.Variable, name.text);
        collect_declarations(symbols, node.children[1 .. $]);
        break;
    }
}
