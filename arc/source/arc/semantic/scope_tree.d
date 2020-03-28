module arc.semantic.scope_tree;

import arc.semantic.symbol: Symbol;
import arc.syntax.ast: AstNode;
import arc.hash: Key;

struct Scope {
    Scope* outer;
    Scope[] inner;
    size_t[Key] declarations;
}

struct ScopeTree {
    Scope* root_scope;
    Symbol[] symbols;
    size_t[] declaration_ids;
}

struct ScopeTreeBuilder {
    ScopeTree scope_tree;
    Scope* current_scope;

    static initialize() {
        auto tree = ScopeTree(new Scope());
        return new ScopeTreeBuilder(tree, tree.root_scope);
    }

    void push_scope() {
        auto s = Scope(current_scope);
        current_scope.inner ~= s;
        current_scope = &current_scope.inner[$ - 1];
    }

    void new_declaration(AstNode* node, Symbol.Kind kind, Key name) {
        const decl_id = scope_tree.symbols.length;
        
        scope_tree.symbols ~= Symbol(kind, name);

        scope_tree.declaration_ids ~= decl_id;
        current_scope.declarations[name] = decl_id;
    }

    void pop_scope() {
        current_scope = current_scope.outer;
    }
}

void collect_declarations(ScopeTreeBuilder *builder, AstNode* node) {
    switch (node.type) with (AstNode.Type) {
        case Define:
            auto name = node.get_children()[0].get_key();
            auto type = node.get_children()[1];
            auto init = node.get_children()[2];
            
            if (init.type == Function) {
                builder.new_declaration(node, Symbol.DefFunction, name);
                builder.push_scope();
                collect_declarations(builder, type);
                collect_declarations(builder, init);
                builder.pop_scope();
                break; // to the loop
            }

            if (init.type != None) {
                builder.new_declaration(node, Symbol.DefConstant, name);
                break; // to the loop
            }

            assert(init.type == None);
            assert(type.type != InferredType);
            builder.new_declaration(node, Symbol.DefType, name);
            break; // to the loop

        case Variable:
            builder.new_declaration(node, Symbol.NewVariable, node.get_children()[0].get_key());
            collect_declarations(builder, node.get_children()[0]);
            collect_declarations(builder, node.get_children()[1]);
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
                builder.new_declaration(param, Symbol.NewVariable, param.get_children()[0].get_key());
            }

            collect_declarations(builder, node.get_children()[2]);

            builder.pop_scope();
            break;

        case Module:
            foreach (stmt; node.get_children())
                collect_declarations(builder, stmt);
            break;

        default:
            break;
    }
}
