module arc.semantic.symbol;

import arc.hash: Key;
import arc.syntax.ast: AstNode;
import arc.semantic.type: Type;

struct ModuleScope {
    Symbol*[AstNode] symbols;
}

struct Symbol {
    enum Kind {
        DefType,
        DefFunction,
        DefConstant,
        NewVariable,
    }

    alias Kind this;

    Kind kind;
    Key name;
    Type* type;
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
                    definitions.symbols[decl] = new Symbol(Symbol.DefFunction, name);
                    continue; // to the loop
                }

                if (init.type != None) {
                    definitions.symbols[decl] = new Symbol(Symbol.DefConstant, name);
                    continue; // to the loop
                }

                assert(init.type == None);
                assert(type.type != InferredType);
                definitions.symbols[decl] = new Symbol(Symbol.DefType, name);
                continue; // to the loop

            case Variable:
                definitions.symbols[decl] = new Symbol(Symbol.NewVariable, decl.get_children()[0].get_key());
                continue;
            default:
                continue;
        }
    }

    return definitions;
}
