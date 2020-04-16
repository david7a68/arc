module arc.semantic.symbol;

import arc.hash: Key;
import arc.ast: AstNode;
import arc.semantic.type: ArcType;

abstract class Symbol {
    enum Kind {
        None,
        Type,
        Function,
        Constant,
        Variable,
        Alias,
        Access,
        Reference,
        TypeMember,
    }

    alias Kind this;

    Kind kind;
    Key name;
    ArcType* type;

    this(Kind kind, Key name, ArcType* type) {
        this.kind = kind;
        this.name = name;
        this.type = type;
    }
}

final class TypeSymbol : Symbol {
    this(Key name, ArcType* type) {
        super(Type, name, type);
    }
}

final class FunctionSymbol : Symbol {
    AstNode* body;

    this(Key name, ArcType* type, AstNode* body) {
        super(Function, name, type);
        this.body = body;
    }
}

alias ConstantSymbol = ValueSymbol!(Symbol.Constant);
alias VariableSymbol = ValueSymbol!(Symbol.Variable);

final class ValueSymbol(Symbol.Kind kind) : Symbol {
    AstNode* value;

    this(Key name, ArcType* type, AstNode* value) {
        super(kind, name, type);
        this.value = value;
    }
}

alias AliasSymbol = DerivedSymbol!(Symbol.Alias);
alias AccessSymbol = DerivedSymbol!(Symbol.Access);
alias ReferenceSymbol = DerivedSymbol!(Symbol.Reference);
alias TypeMemberSymbol = DerivedSymbol!(Symbol.TypeMember);

final class DerivedSymbol(Symbol.Kind kind) : Symbol {
    Symbol parent;

    this(Key name, ArcType* type, Symbol parent) {
        super(kind, name, type);
        this.parent = parent;
    }
}
