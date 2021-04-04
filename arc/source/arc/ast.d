module arc.ast;

import shard.pad : pad_bytes;

alias NodeIndex = uint;

struct SyntaxTree {
    // dfmt off
    const char[]    source;
    Array!Token     tokens;
    Array!AstNode   nodes;
    Array!NodeIndex node_data;
    // dfmt on
}

struct AstNode {
    enum Kind : ubyte {
        invalid,

        /// data_a: [name, type, value]
        /// data_b: next declaration
        let_decl,
        /// ditto
        def_decl,

        identifier,

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
    mixin pad_bytes!3;
    NodeIndex data_a;
    NodeIndex data_b;

    static assert(AstNode.sizeof == 12);
}
