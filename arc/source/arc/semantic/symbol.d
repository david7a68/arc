module arc.semantic.symbol;

import arc.hash: Key;
import arc.syntax.ast: AstNode;
import arc.semantic.type: Type;

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
