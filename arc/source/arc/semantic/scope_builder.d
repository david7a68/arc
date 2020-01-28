module arc.semantic.scope_builder;

import arc.semantic.symbol: Symbol;

struct ScopeBuilder {
    import arc.compiler: CompilerContext;
    import arc.syntax.ast: AstNode;
    import arc.hash: Key;

    CompilerContext* context;
    Symbol* module_scope;
    Symbol* current_scope;
    Symbol*[] unresolved_symbols;

    void enter_scope(AstNode syntax) {
        auto nested = new Symbol(Symbol.Scope, next_slot_index(), current_scope, syntax);
        current_scope.symbols ~= nested;
        current_scope = nested;
    }

    void enter_function_scope(AstNode syntax) {
        auto nested = new Symbol(Symbol.Function, next_slot_index(), current_scope, syntax);
        current_scope.symbols ~= nested;
        current_scope = nested;
    }

    void exit_scope() {
        current_scope = current_scope.parent;
    }

    uint next_slot_index() {
        return cast(uint) current_scope.symbols.length;
    }

    void define(AstNode syntax, Key key, Symbol.Kind kind) {
        assert(kind != Symbol.Scope, "Use enter_scope() to define a block!");
        assert(current_scope.kind == Symbol.Scope, "This should never be false, but just in case.");

        auto symbol = new Symbol(kind, next_slot_index(), current_scope, syntax);
        symbol.key = key;
        current_scope.symbols ~= symbol;
    }

    void unresolved_lookup(AstNode syntax, Key key) {
        auto ref_symbol = new Symbol(Symbol.Lookup, next_slot_index(), current_scope, syntax);
        ref_symbol.key = key;
        current_scope.symbols ~= ref_symbol;
    }
}

import arc.syntax.ast: AstNode;

void build_symbol_tree(ref ScopeBuilder context, AstNode[] nodes...) {
    foreach (node; nodes) {
        switch (node.type) with (AstNode.Type) {
            case Define:
                assert(node.get_children().length == 3);
                context.define(node, node.get_children()[0].get_key(), Symbol.Definition);
                
                build_symbol_tree(context, node.get_children()[1 .. $]);
                break;
            case Variable:
                assert(node.get_children().length == 3);
                context.define(node, node.get_children()[0].get_key(), Symbol.Variable);

                build_symbol_tree(context, node.get_children()[1 .. $]);
                break;
            case Function:
                context.enter_function_scope(node);
                build_symbol_tree(context, node.get_children());
                context.exit_scope();
                break;
            case Block:
                context.enter_scope(node);
                build_symbol_tree(context, node.get_children());
                context.exit_scope();
                break;
            case TypeList:
                context.enter_scope(node);
                build_symbol_tree(context, node.get_children());
                context.exit_scope();
                break;
            case TypeListMember:
                if (node.get_children()[0].get_key() != 0)
                    context.define(node, node.get_children()[0].get_key(), Symbol.Definition);

                build_symbol_tree(context, node.get_children()[1 .. $]);
                break;
            case List:
                context.enter_scope(node);
                build_symbol_tree(context, node.get_children());
                context.exit_scope();
                break;
            case ListMember:
                if (node.get_children()[0].get_key() != 0)
                    context.define(node, node.get_children()[0].get_key(), Symbol.Definition);

                build_symbol_tree(context, node.get_children()[1 .. $]);
                break;
            case Name:
                context.unresolved_lookup(node, node.get_key());
                break;
            default:
                if (node.get_children().length > 0)
                    build_symbol_tree(context, node.get_children());
        }
    }
}
