module arc.data.ast;

import arc.data.hash : Hash;
import arc.data.span;
import arc.data.symbol : ScopedSymbolTable, Symbol;
import arc.data.type : ArcType;

struct AstNode {
    enum Kind : ubyte {
        None,
        Invalid,
        Inferred,

        SymbolRef,
        Import,

        // Literals
        Integer,
        Char,
        String,

        // Unary Operators
        Not,
        Negate,
        PointerType,

        // Binary Operators
        Assign,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,
        Equal,
        NotEqual,
        And,
        Or,
        Call,
        Access,
        StaticAccess,

        // Functions
        Function,
        FunctionType,

        // Sequences
        List,
        Block,
        Loop,

        // Declarations
        ListMember,
        Definition,
        Variable,

        // Control Flow
        If,
        Return,
        Break,
        Continue,
    }

    mixin ast_header!();
}

mixin template ast_header() {
    AstNode.Kind kind;
    Span span;

    ArcType* type;
    ScopedSymbolTable* enclosing_scope;

    AstNode* header() inout return  {
        return cast(AstNode*)&kind;
    }

    bool is_valid() {
        return kind != AstNode.Kind.Invalid;
    }
}

mixin template arity(Names...) {
    union {
        AstNode*[Names.length] parts;
        struct {
            static foreach (name; Names)
                mixin("AstNode* " ~ name ~ ";");
        }
    }
}

struct None {
    mixin ast_header!();
}

struct Inferred {
    mixin ast_header!();
}

struct Invalid {
    mixin ast_header!();

    this(Span span) {
        kind = AstNode.Kind.Invalid;
        this.span = span;
    }
}

alias IntLiteral = Value!(AstNode.Kind.Integer, ulong);
alias StrLiteral = Value!(AstNode.Kind.String, Hash);
alias CharLiteral = Value!(AstNode.Kind.Char, dchar);

struct Value(AstNode.Kind node_kind, T) {
    mixin ast_header!();

    T value;

    this(Span location, T value) {
        kind = node_kind;
        span = location;
        this.value = value;
    }
}

struct SymbolRef {
    mixin ast_header!();

    Hash text;
    Symbol* declaration;

    this(Span location, Hash key) {
        kind = AstNode.Kind.SymbolRef;
        span = location;
        text = key;
    }
}

struct Import {
    mixin ast_header!();

    mixin arity!"call_path";
    ScopedSymbolTable* imported_file;

    this(Span span, AstNode* call_path) {
        kind = AstNode.Kind.Import;
        this.span = span;
        this.call_path = call_path;
    }
}

struct UnOp {
    mixin ast_header!();

    mixin arity!"operand";

    this(AstNode.Kind op_kind, Span prefix, AstNode* operand) {
        kind = op_kind;
        span = prefix + operand.span;
        this.operand = operand;
    }
}

struct BinOp {
    mixin ast_header!();

    mixin arity!("lhs", "rhs");

    this(AstNode.Kind op_kind, AstNode* lhs, AstNode* rhs) {
        kind = op_kind;
        span = lhs.span + rhs.span;
        parts = [lhs, rhs];
    }
}

struct Function {
    mixin ast_header!();

    mixin arity!("parameters", "result", "body");

    this(AstNode* parameters, AstNode* result, AstNode* body) {
        kind = AstNode.Kind.Function;
        span = parameters.span + result.span + body.span;
        parts = [parameters, result, body];
    }
}

struct FunctionSignature {
    mixin ast_header!();

    mixin arity!("parameters", "result");

    this(AstNode* parameters, AstNode* result) {
        kind = AstNode.Kind.FunctionType;
        span = parameters.span + result.span;
        parts = [parameters, result];
    }
}

struct List {
    mixin ast_header!();

    ScopedSymbolTable* scope_;
    AstNode*[] members;

    this(Span span, ScopedSymbolTable* scope_, AstNode*[] members) {
        kind = AstNode.Kind.List;
        this.span = span;
        this.scope_ = scope_;
        this.members = members;
    }
}

struct Block {
    mixin ast_header!();

    ScopedSymbolTable* scope_;
    AstNode*[] statements;

    this(Span span, ScopedSymbolTable* scope_, AstNode*[] statements) {
        kind = AstNode.Kind.Block;
        this.span = span;
        this.scope_ = scope_;
        this.statements = statements;
    }
}

struct Loop {
    mixin ast_header!();

    ScopedSymbolTable* scope_;
    AstNode*[] statements;

    this(Span span, ScopedSymbolTable* scope_, AstNode*[] statements) {
        kind = AstNode.Kind.Loop;
        this.span = span;
        this.scope_ = scope_;
        this.statements = statements;
    }
}

struct Declaration {
    mixin ast_header!();

    Symbol* symbol;
    mixin arity!("type_expr", "init_expr");

    this(AstNode.Kind kind, Span prefix, Symbol* symbol, AstNode* type, AstNode* value) {
        this.kind = kind;
        this.span = prefix + type.span + value.span;
        this.symbol = symbol;
        parts = [type, value];
    }

    this(AstNode.Kind kind, AstNode* type, AstNode* value)
    in (kind == AstNode.Kind.ListMember) {
        this.kind = kind;
        this.span = type.span + value.span;
        parts = [type, value];
    }
}

struct If {
    mixin ast_header!();

    mixin arity!("condition", "pass_branch", "fail_branch");

    this(Span prefix, AstNode* condition, AstNode* pass, AstNode* fail) {
        kind = AstNode.Kind.If;
        span = prefix + condition.span + pass.span + fail.span;
        parts = [condition, pass, fail];
    }
}

struct Return {
    mixin ast_header!();

    mixin arity!("value");

    this(Span prefix, AstNode* value) {
        kind = AstNode.Kind.Return;
        span = prefix + value.span;
        this.value = value;
    }
}

struct Break {
    mixin ast_header!();

    this(Span location) {
        kind = AstNode.Kind.Break;
        span = location;
    }
}

struct Continue {
    mixin ast_header!();

    this(Span location) {
        kind = AstNode.Kind.Continue;
        span = location;
    }
}

AstNode* inferred() {
    return cast(AstNode*) _inferred.header;
}

private const _inferred = Inferred(AstNode.Kind.Inferred);

AstNode* none() {
    return cast(AstNode*) _none.header;
}

private const _none = None(AstNode.Kind.None);

bool verify(AstNode* node) {
    if (node && node.is_valid)
        return node.match!bool(
                (Declaration* n) => n.type_expr != n.init_expr && n.parts.are_valid(),
                (AstNode* n) => node.children.length == 0 || node.children.are_valid());

    return false;
}

bool are_valid(AstNode*[] nodes) {
    import std.algorithm : map, fold;

    // dfmt off
    debug return nodes.map!(n => verify(n)).fold!((a, b) => a && b);
    else return nodes.map!(n => n.is_valid).fold!((a, b) => a && b);
    // dfmt on
}

AstNode*[] children(AstNode* node) {
    if (!node)
        return [];

    // dmft off
    return node.match!(AstNode*[])(
            (Import* n) => n.parts[],
            (UnOp* n) => n.parts[],
            (BinOp* n) => n.parts[],
            (Function* n) => n.parts[],
            (FunctionSignature* n) => n.parts[],
            (List* n) => n.members[],
            (Block* n) => n.statements[],
            (Loop* n) => n.statements[],
            (Declaration* n) => n.parts[],
            (If* n) => n.parts[],
            (Return* n) => n.parts[],
            (AstNode* n) => cast(AstNode*[])[]);
    // dfmt on
}

auto match(RetType, Ops...)(AstNode* node, Ops ops) if (Ops.length > 0) {
    import std.traits : Parameters, ReturnType;
    import std.meta : staticIndexOf, staticMap, AliasSeq;

    static dispatch(T)(AstNode* n, Ops ops) {
        enum int i = staticIndexOf!(T*, staticMap!(Parameters, Ops));
        static if (i >= 0)
            return ops[i](cast(T*) n);
        else static if (is(Parameters!(ops[$ - 1]) == AliasSeq!(AstNode*)))
            return ops[$ - 1](n);
        else static if (ops.length > 0 && !is(RetType == void))
            return RetType.init;
        else
            return;
    }

    if (node is null) {
        static if (ops.length > 0 && !is(RetType == void))
            return RetType.init;
        else
            return;
    }

    // dfmt off
    switch (node.kind) {
        case AstNode.Kind.Invalid:      return dispatch!Invalid(node, ops);
        case AstNode.Kind.None:         return dispatch!None(node, ops);
        case AstNode.Kind.Inferred:     return dispatch!Inferred(node, ops);

        case AstNode.Kind.SymbolRef:    return dispatch!SymbolRef(node, ops);
        case AstNode.Kind.Import:       return dispatch!Import(node, ops);

        case AstNode.Kind.Char:         return dispatch!CharLiteral(node, ops);
        case AstNode.Kind.String:       return dispatch!StrLiteral(node, ops);
        case AstNode.Kind.Integer:      return dispatch!IntLiteral(node, ops);

        case AstNode.Kind.Not: .. case AstNode.Kind.PointerType:
                                        return dispatch!UnOp(node, ops);

        case AstNode.Kind.Assign: .. case AstNode.Kind.StaticAccess:
                                        return dispatch!BinOp(node, ops);

        case AstNode.Kind.Function:     return dispatch!Function(node, ops);
        case AstNode.Kind.FunctionType: return dispatch!FunctionSignature(node, ops);
        
        case AstNode.Kind.ListMember: .. case AstNode.Kind.Variable:
                                        return dispatch!Declaration(node, ops);

        case AstNode.Kind.List:         return dispatch!List(node, ops);
        case AstNode.Kind.Block:        return dispatch!Block(node, ops);
        case AstNode.Kind.Loop:         return dispatch!Loop(node, ops);
        case AstNode.Kind.If:           return dispatch!If(node, ops);
        case AstNode.Kind.Return:       return dispatch!Return(node, ops);
        case AstNode.Kind.Break:        return dispatch!Break(node, ops);
        case AstNode.Kind.Continue:     return dispatch!Continue(node, ops);
        default: assert(false);
    }
    // dfmt on
}
