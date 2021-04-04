module arc.ast;

import shard.array : Array;
import shard.pad : pad_bytes;
import arc.lexer : Token;

alias NodeIndex = uint;
alias TokenIndex = uint;

struct SyntaxTree {
    // dfmt off
    const char[]    source;
    Array!Token     tokens;
    Array!AstNode   nodes;
    Array!NodeIndex node_data;
    Array!AstError  errors;
    // dfmt on
}

struct AstError {
    enum Kind : ubyte {
        token_expect_mismatch
    }

    Kind kind;
    TokenIndex token_index;
    TokenType expected_type;

    static AstError token_expect_mismatch(TokenIndex index, TokenType expected) {
        return AstError(Kind.token_expect_mismatch, index, expected);
    }
}

struct AstNode {
    enum Kind : ubyte {
        invalid,

        /// data_a: [name_hash_lo, name_hash_hi]
        /// data_b: [type, value]
        let_decl,
        /// ditto
        def_decl,

        /// data_a: name token
        /// data_b: intern id
        identifier,

        /// data_a: token index
        int_literal,
        /// ditto
        char_literal,
        /// ditto
        string_literal,

        /// data_a: operand
        /// data_b: unused
        not,
        /// ditto
        negate,

        /// data_a: lhs
        /// data_b: rhs
        assign,
        /// ditto
        add,
        /// ditto
        subtract,
        /// ditto
        multiply,
        /// ditto
        divide,
        /// ditto
        power,
        /// ditto
        modulus,
        /// ditto
        less,
        /// ditto
        less_equal,
        /// ditto
        greater,
        /// ditto
        greater_equal,
        /// ditto
        equal,
        /// ditto
        not_equal,

        /// ditto
        and,
        /// ditto
        or,

        /// data_a: callee
        /// data_b: argument list
        call,

        /// data_a: parameter list
        /// data_b: return type
        function_type,

        /// data_a: [parameter list, return type]
        /// data_b: body
        function_literal,

        /// data_a: first member
        /// data_b: # of members
        list,

        /// data_a: name
        /// data_b: [type, value]
        list_member,

        /// data_a: first statement
        /// data_b: # of statements
        block,

        /// data_a: first statement
        /// data_b: # of statements
        loop,

        /// data_a: result value
        /// data_b: unused
        break_stmt,
        /// ditto
        continue_stmt,
        /// ditto
        return_stmt,

        /// data_a: first condition
        /// data_b: # of conditions
        if_stmt,

        /// data_a: condition
        /// data_b: body
        conditional_pair,
    }

    Kind kind;
    mixin pad_bytes!1;

    ushort length;
    uint source_offset;

    NodeIndex data_a;
    NodeIndex data_b;

    static assert(AstNode.sizeof == 16);

    static identifier(Token t, TokenIndex index) {
        auto n = AstNode(Kind.identifier, t);
        n.data_a = index;
        return n;
    }

    static literal(string s)(Token t, TokenIndex index) {
        mixin("n = AstNode(Kind." ~ s ~ "_literal, t);");
        n.data_a = index;
        return n;
    }

    static unary(string s)(Token t, TokenIndex operand_index) {
        mixin("n = AstNode(Kind." ~ s ~ "_literal, t");
        n.data_a = operand_index;
        return n;
    }

    static binary(string s)(NodeIndex lhs, NodeIndex rhs, uint span_start, uint span_end) {
        mixin("n = AstNode(Kind." ~ s ~ ", span_start, span_end");
        n.data_a = lhs;
        n.data_b = rhs;
        return n;
    }

    this(AstNode.Kind kind, uint span_start, uint span_end) {
        this.kind = kind;
        source_offset = span_start;
        length = span_end - span_start;
    }
}
