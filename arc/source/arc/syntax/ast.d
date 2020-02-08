module arc.syntax.ast;

import arc.hash: Key;
import arc.syntax.location: Span;

abstract class AstNode {
    enum Type : ubyte {
        Invalid,
        None,
        Module,
        // Statements
        Define,
        If,
        Loop,
        Break,
        Return,
        Continue,
        // Expressions
        Name,
        Integer,
        Char,
        List,
        ListMember,
        Block,
        Negate,
        Falsify,
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
        Function,
        Variable,
        // Type expressions
        InferredType,
        PointerType,
        TypeList,
        TypeListMember,
        FunctionType
    }

    alias Type this;

    const Type type;
    const Span span;

    this(Type type, Span span) pure {
        this.type = type;
        this.span = span;
    }

    Key get_key() pure { return 0; }

    AstNode[] get_children() pure { return []; }

    static none() pure {
        import arc.traits: assume_pure;

        static get() { return none_sentinel; }
        static immutable fn = assume_pure(&get);

        return fn();
    }

    static None none_sentinel;
}

bool is_marker(AstNode.Type type) pure {
    return type == AstNode.None || type == AstNode.InferredType;
}

final class None : AstNode {
    this() pure {
        super(AstNode.None, Span());
    }
}

final class Invalid : AstNode {
    this(Span span) pure {
        super(AstNode.Invalid, span);
    }
}

// ----------------------------------------------------------------------
//    _____  _          _                                 _        
//   / ____|| |        | |                               | |       
//  | (___  | |_  __ _ | |_  ___  _ __ ___    ___  _ __  | |_  ___ 
//   \___ \ | __|/ _` || __|/ _ \| '_ ` _ \  / _ \| '_ \ | __|/ __|
//   ____) || |_| (_| || |_|  __/| | | | | ||  __/| | | || |_ \__ \
//  |_____/  \__|\__,_| \__|\___||_| |_| |_| \___||_| |_| \__||___/
//
// ----------------------------------------------------------------------

abstract class Statement : AstNode {
    this(AstNode.Type type, Span span) pure {
        super(type, span);
    }
}

alias Module = StatementSeq!(AstNode.Module, AstNode);
alias Block = StatementSeq!(AstNode.Block, Expression);

final class StatementSeq(AstNode.Type node_type, ParentType) : ParentType {
    AstNode[] statements;

    this(Span span, AstNode[] statements) pure {
        super(node_type, span);
        this.statements = statements;
    }

    override AstNode[] get_children() { return statements; }
}

final class Define : Statement {
    AstNode[3] parts;

    this(Span span, AstNode name, AstNode type, AstNode value) pure {
        super(AstNode.Define, span);
        parts = [name, type, value];
    }

    override AstNode[] get_children() { return parts; }
}

final class If : Statement {
    AstNode[3] parts;

    this(Span span, AstNode condition, AstNode body, AstNode else_branch) pure {
        super(AstNode.If, span);
        parts = [condition, body, else_branch];
    }

    override AstNode[] get_children() { return parts; }
}

final class Break : Statement {
    this(Span span) pure {
        super(AstNode.Break, span);
    }
}

final class Return : Statement {
    AstNode[1] value;

    this(Span span, AstNode return_value) pure {
        super(AstNode.Return, span);
        value[0] = return_value;
    }

    override AstNode[] get_children() { return value; }
}

final class Continue : Statement {
    this(Span span) pure {
        super(AstNode.Continue, span);
    }
}

final class Loop : Statement {
    AstNode[1] body;

    this(Span span, AstNode body) pure {
        super(AstNode.Loop, span);
        this.body[0] = body;
    }

    override AstNode[] get_children() { return body; }
}

// ----------------------------------------------------------------------
//   ______                                    _                    
//  |  ____|                                  (_)                   
//  | |__   __  __ _ __   _ __  ___  ___  ___  _   ___   _ __   ___ 
//  |  __|  \ \/ /| '_ \ | '__|/ _ \/ __|/ __|| | / _ \ | '_ \ / __|
//  | |____  >  < | |_) || |  |  __/\__ \\__ \| || (_) || | | |\__ \
//  |______|/_/\_\| .__/ |_|   \___||___/|___/|_| \___/ |_| |_||___/
//                | |                                               
//                |_|                                               
// ----------------------------------------------------------------------

abstract class Expression : Statement {
    this(AstNode.Type type, Span span) pure {
        super(type, span);
    }
}

alias Name = KeyNode!(AstNode.Name);
alias Char = KeyNode!(AstNode.Char);
alias Integer = KeyNode!(AstNode.Integer);

final class KeyNode(AstNode.Type node_type, ParentType = Expression) : ParentType {
    Key key;

    this(Span span, Key string_key) pure {
        super(node_type, span);
        key = string_key;
    }

    override Key get_key() { return key; }
}

alias Negate = Unary!(AstNode.Negate);
alias Falsify = Unary!(AstNode.Falsify);

alias GetRef = Unary!(AstNode.GetRef);
alias Pointer = Unary!(AstNode.Pointer);

final class Unary(AstNode.Type node_type) : Expression {
    AstNode[1] operand;

    this(Span span, AstNode operand) pure {
        super(node_type, span);
        this.operand[0] = operand;
    }

    override AstNode[] get_children() { return operand; }
}

alias Assign        = Binary!(AstNode.Assign);
alias Less          = Binary!(AstNode.Less);
alias LessEqual     = Binary!(AstNode.LessEqual);
alias Greater       = Binary!(AstNode.Greater);
alias GreaterEqual  = Binary!(AstNode.GreaterEqual);
alias Equal         = Binary!(AstNode.Equal);
alias NotEqual      = Binary!(AstNode.NotEqual);
alias And           = Binary!(AstNode.And);
alias Or            = Binary!(AstNode.Or);
alias Add           = Binary!(AstNode.Add);
alias Subtract      = Binary!(AstNode.Subtract);
alias Multiply      = Binary!(AstNode.Multiply);
alias Divide        = Binary!(AstNode.Divide);
alias Power         = Binary!(AstNode.Power);
alias Call          = Binary!(AstNode.Call);

final class Binary(AstNode.Type node_type) : Expression {
    AstNode[2] operands;

    this(Span span, AstNode lhs, AstNode rhs) pure {
        super(node_type, span);
        operands = [lhs, rhs];
    }

    override AstNode[] get_children() { return operands; }
}

final class List : Expression {
    static final class Member : AstNode {
        AstNode[3] parts;

        this(Span span, AstNode name, AstNode member_type, AstNode value) pure {
            super(AstNode.ListMember, span);
            parts = [name, member_type, value];
        }

        override AstNode[] get_children() { return parts; }
    }

    AstNode[] members;

    this(Span span, AstNode[] members) pure {
        super(AstNode.List, span);
        this.members = members;
    }

    override AstNode[] get_children() { return cast(AstNode[]) members; }
}

final class Function : Expression {
    AstNode[3] parts;

    this(Span span, AstNode parameters, AstNode return_type, AstNode body) pure {
        super(AstNode.Function, span);
        parts = [parameters, return_type, body];
    }

    override AstNode[] get_children() { return parts; }
}

final class Variable : Expression {
    AstNode[3] parts;

    this(Span span, AstNode name, AstNode type, AstNode value) pure {
        super(AstNode.Variable, span);
        parts = [name, type, value];
    }

    override AstNode[] get_children() { return parts; }
}

// ----------------------------------------------------------------------
//   _______                       
//  |__   __|                      
//     | | _   _  _ __    ___  ___ 
//     | || | | || '_ \  / _ \/ __|
//     | || |_| || |_) ||  __/\__ \
//     |_| \__, || .__/  \___||___/
//          __/ || |               
//         |___/ |_|               
// ----------------------------------------------------------------------

abstract class TypeExpression : AstNode {
    this(AstNode.Type type, Span span) pure {
        super(type, span);
    }

    static inferred() {
        import arc.traits: assume_pure;

        static get() { return inferred_sentinel; }
        static immutable fn = assume_pure(&get);

        return fn();
    }

    static TypeExpression inferred_sentinel;
}

final class InferredType : TypeExpression {
    this() pure {
        super(AstNode.InferredType, Span());
    }
}

final class PointerType : TypeExpression {
    AstNode[1] target;

    this(Span span, AstNode target) pure {
        super(AstNode.PointerType, span);
        this.target[0] = target;
    }

    override AstNode[] get_children() { return target; }
}

final class TypeList : TypeExpression {
    static final class Member : AstNode {
        AstNode[2] parts;

        this(Span span, AstNode name, AstNode type) pure {
            super(AstNode.TypeListMember, span);
            parts = [name, type];
        }

        override AstNode[] get_children() { return parts; }
    }

    AstNode[] members;

    this(Span span, AstNode[] members) pure {
        super(AstNode.TypeList, span);
        this.members = members;
    }

    override AstNode[] get_children() { return cast(AstNode[]) members; }
}

final class FunctionType : TypeExpression {
    AstNode[2] parts;

    this(Span span, AstNode parameters, AstNode return_type) pure {
        super(AstNode.FunctionType, span);
        parts = [parameters, return_type];
    }

    override AstNode[] get_children() { return parts; }
}

// ----------------------------------------------------------------------
//    _____  _          _    _         _______  _      _      
//   / ____|| |        | |  (_)       |__   __|| |    (_)     
//  | (___  | |_  __ _ | |_  _   ___     | |   | |__   _  ___ 
//   \___ \ | __|/ _` || __|| | / __|    | |   | '_ \ | |/ __|
//   ____) || |_| (_| || |_ | || (__     | |   | | | || |\__ \
//  |_____/  \__|\__,_| \__||_| \___|    |_|   |_| |_||_||___/
//
// ----------------------------------------------------------------------

static this() {
    AstNode.none_sentinel = new None();
    TypeExpression.inferred_sentinel = new InferredType();
}
