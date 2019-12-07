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

    void enter_scope(AstNode* syntax) {
        auto nested = new Symbol(Symbol.Scope, next_slot_index(), current_scope, syntax);
        current_scope.symbols ~= nested;
        current_scope = nested;
    }

    void exit_scope() {
        current_scope = current_scope.parent;
    }

    uint next_slot_index() {
        return cast(uint) current_scope.symbols.length;
    }

    void define(AstNode* syntax, Key key, Symbol.Kind kind) {
        assert(kind != Symbol.Scope, "Use enter_scope() to define a block!");
        assert(current_scope.kind == Symbol.Scope, "This should never be false, but just in case.");

        auto symbol = new Symbol(kind, next_slot_index(), current_scope, syntax);
        symbol.key = key;
        current_scope.symbols ~= symbol;
    }

    void unresolved_lookup(AstNode* syntax, Key key) {
        auto ref_symbol = new Symbol(Symbol.Lookup, next_slot_index(), current_scope, syntax);
        ref_symbol.key = key;
        current_scope.symbols ~= ref_symbol;
    }
}

import arc.syntax.ast: AstNode;

void build_symbol_tree(ref ScopeBuilder context, AstNode*[] nodes...) {
    foreach (node; nodes) {
        switch (node.type) with (AstNode.Type) {
            case Define:
                assert(node.children.length == 3);
                context.define(node, node.children[0].key, Symbol.Definition);

                if (node.children[1].type == FunctionType && node.children[2].type != None) {
                    check_function_declaration(node.children[1], node.children[2]);
                }
                else if (node.children[1].type == None && node.children[2].type == Function) {
                    // define -> [name, type, value].value -> params
                    auto params = node.children[2].children[0].children;
                    foreach (ref param; params) {
                        if (param.children[0].type == None && param.children[2].type != None) {
                            assert(param.children[1].type == None);

                            auto temp = param.children[0];
                            param.children[0] = param.children[2];
                            param.children[2] = temp;
                        }
                    }
                }
                
                build_symbol_tree(context, node.children[1 .. $]);
                break;
            case VarExpression:
                assert(node.children.length == 3);
                context.define(node, node.children[0].key, Symbol.Variable);

                if (node.children[1].type == FunctionType && node.children[2].type != None) {
                    check_function_declaration(node.children[1], node.children[2]);
                }

                build_symbol_tree(context, node.children[1 .. $]);
                break;
            case Function:
                context.enter_scope(node);
                build_symbol_tree(context, node.children);
                context.exit_scope();
                break;
            case Block:
                context.enter_scope(node);
                build_symbol_tree(context, node.children);
                context.exit_scope();
                break;
            case TypeList:
                context.enter_scope(node);
                build_symbol_tree(context, node.children);
                context.exit_scope();
                break;
            case TypeListMember:
                if (node.children[0].key != 0)
                    context.define(node, node.children[0].key, Symbol.Definition);
                
                build_symbol_tree(context, node.children[1 .. $]);
                break;
            case List:
                context.enter_scope(node);
                build_symbol_tree(context, node.children);
                context.exit_scope();
                break;
            case ListMember:
                if (node.children[0].key != 0)
                    context.define(node, node.children[0].key, Symbol.Definition);
                
                build_symbol_tree(context, node.children[1 .. $]);
                break;
            case Name:
                context.unresolved_lookup(node, node.key);
                break;
            default:
                if (node.num_children > 0)
                    build_symbol_tree(context, node.children);
        }
    }
}

/**
 * Check function declaratations, enforcing the following properties
 * 
 * - If type is not none, it has the same arity as the function
 * - If a parameter has only a value(name), it is a name and not a value
 *   - If it has only a value(name), param.name and param.value will swap
 * - If both the type and the function have functions as return types,
 *     that the above properties apply recursively
 */
void check_function_declaration(AstNode* type, AstNode* func) {
    if (type.type == AstNode.None)
        return;

    assert(func.type == AstNode.Function);

    assert(type.children[0].num_children == func.children[0].num_children);

    // func -> [params, result, body] -> params
    foreach (ref param; func.children[0].children) {
        if (param.children[0].type == AstNode.None) {
            assert(param.children[1].type == AstNode.None);
            assert(param.children[2].type != AstNode.None);

            // swap name and none
            auto none = param.children[0];
            param.children[0] = param.children[2];
            param.children[2] = none;
        }
    }

    if (type.children[1].type == AstNode.FunctionType && func.children[2].type == AstNode.Function) {
        check_function_declaration(type.children[1], func.children[2]);
    }
}
