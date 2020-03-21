module arc.semantic.scope_tree;

import arc.semantic.symbol: Symbol;
import arc.syntax.ast: AstNode;

struct Scope {
    Scope* outer;
    Scope[] inner;
    Symbol*[AstNode] symbols;
}

struct ScopeTree {
    Scope* root_scope;
    Symbol[AstNode] symbols;
}

struct ScopeTreeBuilder {
    ScopeTree scope_tree;
    Scope* current_scope;

    static init() {
        auto tree = ScopeTree(new Scope());
        return new ScopeTreeBuilder(tree, tree.root_scope);
    }

    void push_scope() {
        auto s = Scope(current_scope);
        current_scope.inner ~= s;
        current_scope = &current_scope.inner[$ - 1];
    }

    void pop_scope() {
        current_scope = current_scope.outer;
    }
}

void collect_declarations(ScopeTreeBuilder *builder, AstNode root) {
    foreach (node; root.get_children()) {
        switch (node.type) with (AstNode.Type) {
            case Define:
                auto name = node.get_children()[0].get_key();
                auto type = node.get_children()[1];
                auto init = node.get_children()[2];
                
                if (init.type == Function) {
                    builder.scope_tree.symbols[node] = Symbol(Symbol.DefFunction, name);
                    builder.push_scope();
                    collect_declarations(builder, node);
                    builder.pop_scope();
                    break; // to the loop
                }

                if (init.type != None) {
                    builder.scope_tree.symbols[node] = Symbol(Symbol.DefConstant, name);
                    break; // to the loop
                }

                assert(init.type == None);
                assert(type.type != InferredType);
                builder.scope_tree.symbols[node] = Symbol(Symbol.DefType, name);
                break; // to the loop

            case Variable:
                builder.scope_tree.symbols[node] = Symbol(Symbol.NewVariable, node.get_children()[0].get_key());
                collect_declarations(builder, node);
                break;

            case Block:
                builder.push_scope();

                foreach (statement; node.get_children())
                    collect_declarations(builder, statement);

                builder.pop_scope();
                break;

            case Function:
                builder.push_scope();

                // extract parameter symbols
                auto params = node.get_children()[0];
                foreach (param; params.get_children()) {
                    assert(param.type == ListMember);
                    builder.scope_tree.symbols[param] = Symbol(Symbol.NewVariable, param.get_children()[0].get_key());
                }

                collect_declarations(builder, node.get_children()[2]);

                builder.pop_scope();
                break;

            default:
                continue;
        }
    }
}
