module arc.semantic.symbol;

import arc.syntax.ast: AstNode;

struct ModuleScope {
    Symbol[AstNode] symbols;
}

struct Symbol {
    import arc.hash: Key;

    enum Kind {
        DefType,
        DefFunction,
        DefConstant,
        NewVariable,
    }

    alias Kind this;

    Kind kind;
    Key name;
}

ModuleScope get_module_declarations(AstNode root) {
    ModuleScope definitions;

    foreach (decl; root.get_children()) {
        switch (decl.type) with (AstNode.Type) {
            case Define:
                auto name = decl.get_children()[0].get_key();
                auto type = decl.get_children()[1];
                auto init = decl.get_children()[2];

                if (init.type == Function) {
                    definitions.symbols[decl] = Symbol(Symbol.DefFunction, name);
                    continue; // to the loop
                }

                if (init.type != None) {
                    definitions.symbols[decl] = Symbol(Symbol.DefConstant, name);
                    continue; // to the loop
                }

                assert(init.type == None);
                assert(type.type != InferredType);
                definitions.symbols[decl] = Symbol(Symbol.DefType, name);
                continue; // to the loop

            case Variable:
                definitions.symbols[decl] = Symbol(Symbol.NewVariable, decl.get_children()[0].get_key());
                continue;
            default:
                continue;
        }
    }

    return definitions;
}
