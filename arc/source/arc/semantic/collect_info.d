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
        scope_tree.add_unresolved_symbol(node);
        break;

    case Block:
    case List:
        scope_tree.push_scope();
        foreach (child; node.children)
            collect_semantic_info(scope_tree, child);
        scope_tree.pop_scope();
        break;

    case ListMember:
        if (node.children[0].is_some)
            make_decl_symbol(scope_tree, node.children[0], node.children[1], node.children[2]);

        collect_semantic_info(scope_tree, node.children[1]);
        collect_semantic_info(scope_tree, node.children[2]);
        break;

    case Definition:
    case Variable:
        make_decl_symbol(scope_tree, node.children[0], node.children[1], node.children[2]);
        collect_semantic_info(scope_tree, node.children[1]);
        collect_semantic_info(scope_tree, node.children[2]);
        break;
    }
}

void make_decl_symbol(ScopeBuilder* scope_tree, AstNode* name, AstNode* expr, AstNode* type)
in(name.kind == AstNode.Kind.Name) {
    auto kind = to_symbol_def(expr.is_some ? expr.kind : type.kind);
    auto symbol = scope_tree.add_symbol(kind, name.text);
    name.symbol = symbol;
    name.is_resolved_symbol = true;
}

Symbol.Kind to_symbol_def(AstNode.Kind kind) {
    // dfmt off
    switch (kind) with (AstNode.Kind) {
        case None:
        case Invalid:
        case Inferred:
        case Definition:
        case Variable:
        case If:
        case Return:
        case Break:
        case Continue:
        case Loop:
        case Block:
                            return Symbol.Kind.None;
        case Integer:       return Symbol.Kind.Integer;
        case String:        return Symbol.Kind.String;
        case Char:          return Symbol.Kind.Char;
        case List:          return Symbol.Kind.List;
        case Function:      return Symbol.Kind.Function;
        case FunctionType:  return Symbol.Kind.Function;
        case Import:        return Symbol.Kind.Import;
        default:            return Symbol.Kind.ExprResult;
    }
    // dfmt on
}
