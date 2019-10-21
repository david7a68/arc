module arc.syntax.ast;

import arc.hash: Key;
import arc.syntax.location: Span;

struct AstNode {
    enum Type {
        Invalid,
        None,
        Name,
        Integer,
        Char,
        List,
        Array,
        Block,
        Function,
        Negate,
        Pointer,
        GetRef,
        Assign,
        Less,
        LessEqual,
        Greater,
        GreaterEqual,
        Equal,
        NotEqual,
        And,
        Or,
        Add,
        Subtract,
        Multiply,
        Divide,
        Power,
        Call,
        Path,
        VarExpression,
        Define,
        If,
        Loop,
        Label,
        Break,
        Return,
        Continue,
        Labeled,
    }

    alias type this;

    Type type;
    Span span;

    union {
        Key key;
        ulong value;
        AstNode*[] children;
    }

    this(Type type, Span span) {
        this.type = type;
        this.span = span;
    }

    size_t num_children() {
        switch (type) with (Type) {
            case None:
            case Name:
            case Integer:
            case Char:
            case Label:
                return 0;
            default:
                return children.length;
        }
    }

    static immutable none_value = AstNode(AstNode.None, Span());
    static AstNode* none() { return cast(AstNode*) &none_value; }
}

AstNode* make(T, Args...)(Span span, Args args) {
    return cast(AstNode*) new T(span, args);
}

struct SimpleNode(AstNode.Type node_type) {
    AstNode self;

    this(AstNode other) in (other.type == node_type) {
        self = other;
    }

    this(Span span) {
        self = AstNode(node_type, span);
    }

    Span span() { return self.span; }
    AstNode.Type type() { return self.type; }
}

struct ValueNode(AstNode.Type node_type, ValueType, string value_name) {
    import std.format: format;
    
    AstNode self;
    
    this(AstNode other) in (other.type == node_type) {
        self = other;
    }

    mixin("this(Span span, ValueType %s) {
        self = AstNode(node_type, span);
        self.%s = %s;
    }".format(value_name, value_name, value_name));

    Span span() { return self.span; }
    AstNode.Type type() { return self.type; }

    mixin("ValueType %s() { return self.%s; }".format(value_name, value_name));
}

struct AggregateNode(AstNode.Type node_type, string[] children_names) {
    import std.format: format;

    AstNode self;
    
    this(AstNode other) in (other.type == node_type) {
        self = other;
    }

    this(Span span, AstNode*[] members) {
        self = AstNode(node_type, span);
        self.children = members;
    }

    mixin("this(Span span, %-(AstNode* %s, %)) {
        self = AstNode(node_type, span);
        self.children = [%-(%s, %)];
    }".format(children_names, children_names));

    Span span() { return self.span; }
    AstNode.Type type() { return self.type; }
    AstNode*[] children() { return self.children; }

    static foreach (i, child; children_names) {
        mixin("AstNode* %s() { return children[%s]; }".format(child, i));
        mixin("void %s(AstNode* node) { children[%s] = node; }".format(child, i));
    }
}

struct SeqNode(AstNode.Type node_type) {
    AstNode self;

    this(AstNode other) in (other.type == node_type) {
        self = other;
    }

    this(Span span) {
        self = AstNode(node_type, span);
    }

    this(Span span, AstNode*[] members) {
        self = AstNode(node_type, span);
        self.children = members;
    }

    Span span() { return self.span; }
    AstNode.Type type() { return self.type; }
    AstNode*[] members() { return self.children; }
}

alias Invalid           = SimpleNode!(AstNode.Invalid);
alias None              = SimpleNode!(AstNode.None);
alias Name              = ValueNode!(AstNode.Name, Key, "key");
alias Integer           = ValueNode!(AstNode.Integer, ulong, "value");
alias Char              = ValueNode!(AstNode.Char, Key, "key");
alias List              = SeqNode!(AstNode.List);
alias Array             = SeqNode!(AstNode.Array);
alias Block             = SeqNode!(AstNode.Block);
alias Function          = AggregateNode!(AstNode.Function, ["params", "body"]);
alias Negate            = AggregateNode!(AstNode.Negate, ["operand"]);
alias Pointer           = AggregateNode!(AstNode.Pointer, ["operand"]);
alias GetRef            = AggregateNode!(AstNode.GetRef, ["operand"]);
alias Assign            = AggregateNode!(AstNode.Assign, ["lhs", "expression"]);
alias Less              = AggregateNode!(AstNode.Less, ["lhs", "rhs"]);
alias LessEqual         = AggregateNode!(AstNode.LessEqual, ["lhs", "rhs"]);
alias Greater           = AggregateNode!(AstNode.Greater, ["lhs", "rhs"]);
alias GreaterEqual      = AggregateNode!(AstNode.GreaterEqual, ["lhs", "rhs"]);
alias Equal             = AggregateNode!(AstNode.Equal, ["lhs", "rhs"]);
alias NotEqual          = AggregateNode!(AstNode.NotEqual, ["lhs", "rhs"]);
alias And               = AggregateNode!(AstNode.And, ["lhs", "rhs"]);
alias Or                = AggregateNode!(AstNode.Or, ["lhs", "rhs"]);
alias Add               = AggregateNode!(AstNode.Add, ["lhs", "rhs"]);
alias Subtract          = AggregateNode!(AstNode.Subtract, ["lhs", "rhs"]);
alias Multiply          = AggregateNode!(AstNode.Multiply, ["lhs", "rhs"]);
alias Divide            = AggregateNode!(AstNode.Divide, ["lhs", "rhs"]);
alias Power             = AggregateNode!(AstNode.Power, ["lhs", "rhs"]);
alias Call              = AggregateNode!(AstNode.Call, ["target", "arguments"]);
alias Path              = AggregateNode!(AstNode.Path, ["base", "name"]);
alias VarExpression     = AggregateNode!(AstNode.VarExpression, ["name", "type", "expression"]);
alias Define            = AggregateNode!(AstNode.Define, ["name", "type", "expression"]);
alias If                = AggregateNode!(AstNode.If, ["condition", "body", "else_"]);
alias Loop              = AggregateNode!(AstNode.Loop, ["body"]);
alias Label             = ValueNode!(AstNode.Label, Key, "key");
alias Break             = AggregateNode!(AstNode.Break, ["label", "value"]);
alias Return            = AggregateNode!(AstNode.Return, ["label", "value"]);
alias Continue          = AggregateNode!(AstNode.Continue, ["label"]);
alias Labeled           = AggregateNode!(AstNode.Labeled, ["label", "expression"]);
