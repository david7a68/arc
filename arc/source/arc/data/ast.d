module arc.data.ast;

import arc.data.source: Span;

/**
 * Why use an abstract class to represent a discriminated union of types?
 *
 * 1) Additional control over what goes into the shared part. The variant type
 *    supported by the standard library does not allow you to choose what goes
 *    into the discriminant part. We want to keep more shared information.
 * 2) POD structs with a shared struct header is a viable option, but classes
 *    already implement much of the functionality expected of variable-sized
 *    discriminated unions.
 * 3) Other implementations for discriminated unions tend to pad to the largest
 *    type, wasting space that it didn't need to.
 */
abstract class AstNode {
    import arc.data.type: ArcType;

    enum Kind : ubyte {
        Invalid,
        None,
        Inferred,
        ConstantDeclaration,
        TypeDeclaration,
        Variable,
        Name,
        Integer,
        Char,
        List,
        Block,
        Unary,
        Binary,
        Call,
        Access,
        Function,
        FunctionType,
    }

    const Span span;
    ArcType type;

    const Kind kind;

    static AstNode none, inferred;

    this(Kind kind, Span span) {
        this.kind = kind;
        this.span = span;
    }

    AstNode[] children() { return[]; }

    bool is_valid() const { return kind != Kind.Invalid; }
}

static this() {
    AstNode.none = new None();
    AstNode.inferred = new Inferred();
}

final class Invalid : AstNode {
    this(Span span) {
        super(Kind.Invalid, span);
    }
}

alias None = Marker!(AstNode.Kind.None);
alias Inferred = Marker!(AstNode.Kind.Inferred);

final class Marker(AstNode.Kind marker_kind) : AstNode {
    this() {
        super(marker_kind, Span());
    }
}

alias ConstantDeclaration = ValueDeclaration!(AstNode.Kind.ConstantDeclaration);
alias Variable = ValueDeclaration!(AstNode.Kind.Variable);

final class ValueDeclaration(AstNode.Kind decl_kind) : AstNode {
    AstNode[3] parts;

    this(Span span, AstNode name, AstNode type, AstNode value) {
        super(decl_kind, span);
        parts = [name, type, value];
    }

    override AstNode[] children() { return parts[]; }
}

final class TypeDeclaration : AstNode {
    AstNode[2] parts;

    this(Span span, AstNode name, AstNode type) {
        super(Kind.TypeDeclaration, span);
        parts = [name, type];
    }

    override AstNode[] children() { return parts[]; }
}

alias Block = Sequence!(AstNode.Kind.Block);
alias List = Sequence!(AstNode.Kind.List);

final class Sequence(AstNode.Kind seq_kind) : AstNode {
    AstNode[] members;

    this(Span span, AstNode[] members) {
        super(seq_kind, span);
        this.members = members;
    }

    override AstNode[] children() { return members; }
}

alias Char = Value!(AstNode.Kind.Char);
alias Integer = Value!(AstNode.Kind.Integer);
alias Name = Value!(AstNode.Kind.Name);

final class Value(AstNode.Kind value_kind) : AstNode {
    import arc.data.hash: Key;
    
    Key text;

    this(Span span, Key value_text) {
        super(value_kind, span);
        text = value_text;
    }
}

alias Call = Operator!(AstNode.Kind.Call, 2);
alias Unary = Operator!(AstNode.Kind.Unary, 2);
alias Binary = Operator!(AstNode.Kind.Binary, 3);

final class Operator(AstNode.Kind op_kind, size_t n_parts) : AstNode {
    import arc.data.hash: Key;

    AstNode[n_parts] operands;

    this(AstNode[] operands...) in (n_parts == operands.length) {
        import arc.data.source : merge;
        super(op_kind, operands[0].span.merge(operands[$ - 1].span));
        this.operands[] = operands;
    }

    override AstNode[] children() { return operands[]; }
}

final class Function : AstNode {
    AstNode[3] parts;

    this(Span span, AstNode params, AstNode result, AstNode body) {
        super(Kind.Function, span);
        parts = [params, result, body];
    }

    override AstNode[] children() { return parts[]; }
}

final class FunctionType : AstNode {
    AstNode[2] parts;

    this(Span span, AstNode params, AstNode result) {
        super(Kind.FunctionType, span);
        parts = [params, result];
    }

    override AstNode[] children() { return parts[]; }
}
