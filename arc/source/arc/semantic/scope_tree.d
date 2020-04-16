module arc.semantic.scope_tree;

import arc.semantic.symbol;
import arc.ast: AstNode;
import arc.hash: Key;

struct Scope {
    Scope* outer;
    Scope[] inner;
    size_t[Key] declarations;
    size_t[] reference_ids;

    size_t resolve(Symbol[] symbols, Key name) {
        const index = declarations.get(name, 0);

        if (index != 0)
            return index;

        if (outer !is null)
            return outer.resolve(symbols, name);
        else
            return 0;
    }
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

    // void new_declaration(AstNode* node, Symbol.Kind kind, Key name) {
    //     const decl_id = scope_tree.symbols.length;
        
    //     scope_tree.symbols ~= new Symbol(kind, name);

    //     scope_tree.declaration_ids ~= decl_id;
    //     current_scope.declarations[name] = decl_id;
    // }

    // void new_reference(AstNode* node, Key name) {
    //     const ref_id = scope_tree.symbols.length;

    //     scope_tree.symbols ~= new Symbol(Symbol.Reference, name);

    //     current_scope.reference_ids ~= ref_id;
    // }

    void pop_scope() {
        current_scope = current_scope.outer;
    }
}

Symbol[AstNode*] collect_declarations(AstNode* tree) {
    import arc.semantic.type_analysis: get_rough_type;
    import arc.semantic.type: ArcType;

    Symbol[AstNode*] result;

    foreach (node; tree.get_children())
    switch (node.type) with (AstNode.Type) {
        case Define:
            auto name = node.get_children()[0].get_key();
            auto type = node.get_children()[1];
            auto init = node.get_children()[2];

            auto arc_type = get_rough_type(type, init);

            switch (arc_type.kind) {
                case ArcType.Function:
                    result[node] = new FunctionSymbol(name, arc_type, init); 
                    break;
                case ArcType.Aggregate:
                    result[node] = new TypeSymbol(name, arc_type);
                    break;
                case ArcType.Named:
                    result[node] = new ReferenceSymbol(name, arc_type, null); 
                    break;
                case ArcType.Integer:
                case ArcType.Float:
                case ArcType.Char:
                case ArcType.Unknown:
                    result[node] = new ConstantSymbol(name, arc_type, init); 
                    break;
                default:
                    assert(false);
            }
            break;

        case Variable:
            auto name = node.get_children()[0].get_key();
            auto type = node.get_children()[1];
            auto init = node.get_children()[2];

            auto arc_type = get_rough_type(type, init);

            result[node] = new VariableSymbol(name, arc_type, init);
            break;

        default:
    }

    return result;
}

void collect_symbols(ScopeTreeBuilder *builder, AstNode* node) {
    // switch (node.type) with (AstNode.Type) {
    //     case Define:
    //         auto name = node.get_children()[0].get_key();
    //         auto type = node.get_children()[1];
    //         auto init = node.get_children()[2];
            
    //         if (init.type == Function) {
    //             builder.new_declaration(node, Symbol.DefFunction, name);
    //             builder.push_scope();
    //             collect_symbols(builder, type);
    //             collect_symbols(builder, init);
    //             builder.pop_scope();
    //             break; // to the loop
    //         }

    //         if (init.type != None) {
    //             builder.new_declaration(node, Symbol.DefConstant, name);
    //             collect_symbols(builder, type);
    //             collect_symbols(builder, init);
    //             break; // to the loop
    //         }

    //         assert(init.type == None);
    //         assert(type.type != InferredType);
    //         builder.new_declaration(node, Symbol.DefType, name);
    //         collect_symbols(builder, type);
    //         break; // to the loop

    //     case Variable:
    //         auto name = node.get_children()[0].get_key();
    //         auto type = node.get_children()[1];
    //         auto init = node.get_children()[2];

    //         builder.new_declaration(node, Symbol.NewVariable, name);
    //         collect_symbols(builder, type);
    //         collect_symbols(builder, init);
    //         break;

    //     case Block:
    //         builder.push_scope();

    //         foreach (statement; node.get_children())
    //             collect_symbols(builder, statement);

    //         builder.pop_scope();
    //         break;

    //     case Function:
    //         builder.push_scope();

    //         // extract parameter symbols
    //         auto params = node.get_children()[0];
    //         auto type = node.get_children()[1];
    //         auto body = node.get_children()[2];

    //         foreach (param; params.get_children()) {
    //             assert(param.type == ListMember);   // No invalid members here!

    //             collect_symbols(builder, param);
    //         }

    //         collect_symbols(builder, type);
    //         collect_symbols(builder, body);

    //         builder.pop_scope();
    //         break;

    //     case List:
    //         builder.push_scope();
            
    //         auto members = node.get_children();
    //         foreach (member; members) {
    //             assert(member.type == ListMember);   // No invalid members here!

    //             collect_symbols(builder, member);
    //         }

    //         builder.pop_scope();
    //         break;

    //     case ListMember:
    //         const name = node.get_children()[0].get_key();
    //         auto type = node.get_children()[1];
    //         auto init = node.get_children()[2];

    //         if (name != 0)
    //             builder.new_declaration(node, Symbol.NewVariable, name);

    //         collect_symbols(builder, type);
    //         collect_symbols(builder, init);
    //         break;

    //     case TypeList:
    //         builder.push_scope();

    //         auto members = node.get_children();
    //         foreach (member; members) {
    //             assert(member.type == TypeListMember);

    //             const name = member.get_children()[0].get_key();
    //             auto type = member.get_children()[1];

    //             if (name != 0)
    //                 builder.new_declaration(node, Symbol.NewVariable, name);

    //             collect_symbols(builder, type);
    //         }

    //         builder.pop_scope();
    //         break;

    //     case Access:
    //         auto source = node.get_children()[0];
    //         auto member = node.get_children()[1];

    //         // builder.new_reference(source, source.get_key());
    //         // builder.new_access(member, source.get_key(), member.get_key());
    //         break;

    //     case Name:
    //         builder.new_reference(node, node.get_key());
    //         break;

    //     default:
    //         foreach (stmt; node.get_children())
    //             collect_symbols(builder, stmt);
    //         break;
    // }
}

// void resolve_symbols(Symbol[] symbols, Scope* outermost_scope) {
//     foreach (ref_id; outermost_scope.reference_ids) {
//         auto symbol_id = outermost_scope.resolve(symbols, symbols[ref_id].name);

//         symbols[ref_id].ref_id = symbol_id;
//     }

//     foreach (ref inner; outermost_scope.inner) {
//         resolve_symbols(symbols, &inner);
//     }
// }
