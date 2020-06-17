module arc.semantic.collect_info;

import arc.data.ast;
import arc.data.symbol;
import arc.semantic.scope_tree;
import arc.memory : VirtualMemory;

void collect_semantic_info(ScopeBuilder* scope_tree, AstNode*[] syntax...) {
    foreach (node; syntax) switch (node.kind) with (AstNode.Kind) {
    default:
        collect_semantic_info(scope_tree, node.children);
        break;

    case Name:
        // Unresolved Reference
        scope_tree.add_unresolved_symbol(node);
        break;

    case Block:
    case List:
        // Scope {
        //     Body
        // }
        scope_tree.push_scope();
        foreach (child; node.children)
            collect_semantic_info(scope_tree, child);
        scope_tree.pop_scope();
        break;

    case Function:
        // FunctionScope {
        //     Parameters   (Symbols)
        //     ReturnType
        //     Body
        // }
        auto parameters = node.children[0];
        assert(parameters.kind == AstNode.Kind.List);

        scope_tree.push_scope();
        foreach (param; parameters.children) {
            assert(param.kind == AstNode.Kind.ListMember);
            auto name = param.children[0];
            auto type = param.children[1];
            auto expr = param.children[2];

            auto symbol = scope_tree.add_symbol(Symbol.Kind.FunctionParam, name.text);
            name.symbol = symbol;
            name.is_resolved_symbol = true;

            collect_semantic_info(scope_tree, type);
            collect_semantic_info(scope_tree, expr);
        }

        collect_semantic_info(scope_tree, node.children[1 .. $]);
        scope_tree.pop_scope();
        break;

    case ListMember:
        // ListMember {
        //     Name         (Symbol)
        //     Type
        //     Value
        // }
        if (node.children[0].is_some)
            make_decl_symbol(scope_tree, Symbol.Kind.FunctionParam, node.children[0]);

        collect_semantic_info(scope_tree, node.children[1 .. $]);
        break;

    case Definition:
        make_decl_symbol(scope_tree, Symbol.Kind.Constant, node.children[0]);
        collect_semantic_info(scope_tree, node.children[1 .. $]);
        break;

    case Variable:
        // Declaration {
        //     Name         (Symbol)
        //     Type
        //     Value
        // }
        make_decl_symbol(scope_tree, Symbol.Kind.Variable, node.children[0]);
        collect_semantic_info(scope_tree, node.children[1 .. $]);
        break;
    }
}

void make_decl_symbol(ScopeBuilder* scope_tree, Symbol.Kind kind, AstNode* name)
in(name.kind == AstNode.Kind.Name) {
    auto symbol = scope_tree.add_symbol(kind, name.text);
    name.symbol = symbol;
    name.is_resolved_symbol = true;
}
