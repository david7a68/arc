module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;

struct Parser {
    import arc.syntax.syntax_reporter: SyntaxReporter;
    import arc.stringtable: StringTable;

    ///
    Token token;
    ///
    Lexer lexer;
    ///
    StringTable *table;
    ///
    SyntaxReporter *error;

    ///
    this(const(char)[] source, StringTable* table, SyntaxReporter* error) {
        lexer = Lexer(source, table);
        lexer.ready();
        token = lexer.current;
        this.error = error;
    }

    ///
    void advance() {
        lexer.advance();
        token = lexer.current;
        // import std.stdio; writeln(token);
    }

    ///
    bool consume(Token.Type type) {
        if (type != token.type)
            return false;
        
        advance();
        return true;
    }

    /**
     * Push a new token type to the top of the automatic end-of-line token
     * insertion stack
     */
    void push_eol_type(Token.Type type) {
        lexer.push_eol_type(type);
    }

    /**
     * Pop a token type from the top of the automatic end-of-line token
     * insertion stack
     */
    void pop_eol_type() {
        lexer.pop_eol_type();
    }
}

///
Expression expression(ref Parser p) {
    return expression(p, Infix.Precedence.Assignment);
}

unittest {
    mixin(parser_init!"]");
    bool bad_token;
    parser.error.user_data = &bad_token;
    parser.error.token_cannot_start_expr_impl = report_bad_token_set_flag;
    parser.expression();
    assert(bad_token);
}

alias Prefix = Expression function(ref Parser);

/// List of functions for parsing tokens that can start expressions
immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p;

    p[Token.Lparen] = &list;
    p[Token.Lbracket] = &list;
    p[Token.Name] = &name;
    p[Token.Integer] = &integer;
    p[Token.Minus] = &negate;

    return p;
} ();


/// Convenience struct to hold the precedence and parser for an infix expression
struct Infix {
    enum Precedence {
        None,
        Assignment,
        Equality,
        Comparison,
        Sum,
        Product,
        Power,
        Unary,
        Call,
        FunctionLiteral
    }

    Precedence precedence;

    alias InfixParselet = Expression function(ref Parser, Expression);
    InfixParselet parselet;
    
    alias Precedence this;
}

/// List of functions for parsing infix expressions (binary operators, calls, etc)
immutable Infix[256] infix_parslets = () {
    Infix[256] p;

    p[Token.Rarrow]     = Infix(Infix.FunctionLiteral, &function_);
    p[Token.Plus]       = Infix(Infix.Sum, &add);
    p[Token.Minus]      = Infix(Infix.Sum, &subtract);
    p[Token.Star]       = Infix(Infix.Product, &multiply);
    p[Token.Slash]      = Infix(Infix.Product, &divide);
    p[Token.Caret]      = Infix(Infix.Power, &power);
    p[Token.Lparen]     = Infix(Infix.Call, &call);
    p[Token.Lbracket]   = Infix(Infix.Call, &call);

    return p;
} ();

Expression expression(ref Parser p, Infix.Precedence prec) {
    auto parselet = prefix_parslets[p.token.type];

    if (parselet is null) {
        p.error.token_cannot_start_expr(p.token);
        return new Invalid(p.token.start, p.token.span);
    }
    
    auto subexpression_error = false;
    auto expr = parselet(p);
    while (prec <= infix_parslets[p.token.type].precedence) {
        auto infix = infix_parslets[p.token.type].parselet;

        if (infix !is null) {
            expr = infix(p, expr);
        }
        else {
            p.error.token_is_not_an_operator(expr.start, p.token);
            
            // skip the token, and then the rest of the expression
            p.advance();
            auto _rhs = p.expression();

            expr = new Invalid(expr.start, (_rhs.start + _rhs.span) - expr.start);
            subexpression_error = true;
        }
    }

    if (!subexpression_error)
        return expr;
    
    return new Invalid(expr.start, expr.span);
}

/// Name := ('_' | $a-zA-Z)* ;
Name name(ref Parser p) {
    scope(exit) p.advance();
    return new Name(p.token.start, p.token.span);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
Integer integer(ref Parser p) {
    import std.conv: to;

    scope(exit) p.advance();
    return new Integer(p.token.start, p.token.span, p.token.start[0..p.token.span].to!ulong);
}

unittest {
    mixin(parser_init!("10294"));
    const e = parser.expression();
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Integer);
    assert((cast(Integer) e).value == 10_294);
}

/// List := ListOpen  (','* VarExpression)? ','* ListClose ;
Expression list(ref Parser p) {
    const start = p.token.start;
    const closing_tok = p.token.type == Token.Lbracket ? Token.Rbracket : Token.Rparen;
    p.push_eol_type(Token.Comma);
    scope(exit) p.pop_eol_type();
    p.advance();

    // skip leading commas
    while(p.token.type == Token.Comma)
        p.advance();

    VarExpression[] members;
    bool subexpression_error = false;
    while (p.token.type != closing_tok) {
        auto e = p.var_expr();

        assert(e.type == AstNode.Invalid || e.type == AstNode.VarExpression);
        if (e.type == AstNode.Invalid)
            subexpression_error = true;
        else
            members ~= cast(VarExpression) e;

        if (p.token.type == Token.Comma) {
            do {
                p.advance();
            } while (p.token.type == Token.Comma);
        }
        else if (p.token.type != closing_tok) {
            p.error.list_not_closed(start, p.token.start);
            auto err = new Invalid(start, (p.token.start + p.token.span) - start);
            p.advance();
            return err;
        }
    }

    const close = p.token;
    p.consume(closing_tok);
    if (!subexpression_error) {
        auto list = new List(start, (close.start + close.span) - start);
        list.children = members;
        return list;
    }
    else {
        auto inv = new Invalid(start, (close.start + close.span) - start);
        return inv;
    }
}

unittest {
    {
        mixin(parser_init!("[]"));
        auto e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.List);
        assert(e.children.length == 0);
    }
    {
        mixin(parser_init!("[a\n\n\n(b)]"));
        auto e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.List);
        assert(e.children.length == 2);
        assert(e.children[0].type == AstNode.VarExpression);
        assert(e.children[1].type == AstNode.VarExpression);
    }
    {
        mixin(parser_init!("[a\n)"));
        bool[2] errors; // [0]: cannot start expr, [1]: list not closed
        parser.error.user_data = &errors;
        parser.error.token_cannot_start_expr_impl = (self, tok) {
            (cast(bool[2]*) self.user_data)[0] = true;
        };
        parser.error.list_not_closed_impl = (self, loc, erloc) {
            (cast(bool[2]*) self.user_data)[1] = true;
        };

        const e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.Invalid);
        assert(errors[0] && errors[1]);
    }
    {
        mixin(parser_init!("[a, [b)]"));
        bool not_closed;
        parser.error.user_data = &not_closed;
        parser.error.list_not_closed_impl = report_loc_err_set_flag;

        const e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.Invalid);
        assert(not_closed);
    }
}

/// Negate := '-' Expression
Expression negate(ref Parser p) {
    const start = p.token.start;
    p.consume(Token.Minus);

    auto expr = prefix_parslets[p.token.type](p);
    if (expr.type != AstNode.Invalid)
        return new Negate(expr);

    return new Invalid(start, (expr.start + expr.span) - start);
}

/// Function := Expression '->' Expression ;
Expression function_(ref Parser p, Expression params) {
    p.consume(Token.Rarrow);

    auto body = p.expression();

    if (params.type != AstNode.Invalid && body.type != AstNode.Invalid)
        return new Function(params, body);

    return new Invalid(params.start, (body.start + body.span) - params.start);
}

unittest {
    mixin(parser_init!("() -> ()"));
    auto e = cast(Function) parser.expression();
    assert(parser.token.type == Token.Eof);
    assert(e.parameters.type == AstNode.List);
    assert(e.body.type == AstNode.List);
}

/// Binary := Expression <op> Expression ;
Expression binary(T, int prec, Token.Type ttype)(ref Parser p, Expression lhs) {
    p.consume(ttype);

    auto rhs = p.expression(cast(Infix.Precedence) prec);
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new T(lhs, rhs);
    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

alias add = binary!(Add, Infix.Sum + 1, Token.Plus);
alias subtract = binary!(Subtract, Infix.Sum + 1, Token.Minus);
alias multiply = binary!(Multiply, Infix.Product + 1, Token.Star);
alias divide = binary!(Divide, Infix.Product + 1, Token.Slash);
alias power = binary!(Power, Infix.Power, Token.Caret);

/// Call := Expression List
Expression call(ref Parser p, Expression lhs) {
    auto rhs = p.list();

    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Call(lhs, rhs);
    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

unittest {
    mixin(parser_init!"[][()]");
    auto e = cast(Call) parser.expression();
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Call);
    assert(e.children.length == 2);
    assert(e.target.type == AstNode.List);
    assert(e.arguments.type == AstNode.List);
    assert((cast(VarExpression) e.arguments.children[0]).value_expr.type == AstNode.List);
}

/**
 * var_expr : Expression ((":" Expression ("=" Expression)?) |
 *                        ("=" Expression))?
 */
Expression var_expr(ref Parser p) {
    Expression first, type_expr, value_expr;
    const start = p.token.start;
    const(char)* end = start;
    bool subexpression_error;

    first = p.expression();
    end = first.start + first.span;
    if (first.type == AstNode.Invalid)
        subexpression_error = true;

    if (p.consume(Token.Colon)) {
        // var_expr : Expression (":" Expression)?
        type_expr = p.expression();
        end = type_expr.start + type_expr.span;
        if (type_expr.type == AstNode.Invalid)
            subexpression_error = true;

        if (p.consume(Token.Equals)) {
            // var_expr : Expression (":" Expression ("=" Expression)?)?
            value_expr = p.expression();
            end = value_expr.start + value_expr.span;
            if (value_expr.type == AstNode.Invalid)
                subexpression_error = true;
        }
    }
    else if (p.consume(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        value_expr = p.expression();
        end = value_expr.start + value_expr.span;
        if (value_expr.type == AstNode.Invalid)
                subexpression_error = true;
    }
    else {
        // var_expr : Expression
        value_expr = first;
        first = null;
    }

    if (subexpression_error)
        return new Invalid(start, end - start);
    else
        return new VarExpression(first, type_expr, value_expr, start, end - start);
}

unittest {
    {
        mixin(parser_init!"a");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern is null);
        assert(e.type_expr is null);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        mixin(parser_init!"a:b");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr.type == AstNode.Name);
        assert(e.value_expr is null);
    }
    {
        mixin(parser_init!"a:b=c");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr.type == AstNode.Name);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        mixin(parser_init!"a=c");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr is null);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        mixin(parser_init!"a:=c");
        bool var_error;
        parser.error.user_data = &var_error;
        parser.error.token_cannot_start_expr_impl = report_bad_token_set_flag;
        
        auto e = parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(var_error);
    }
}

version(unittest) {
    import arc.stringtable: StringTable;
    import arc.syntax.syntax_reporter: SyntaxReporter;

    SyntaxReporter.ReportingFunction_loc_err report_loc_err_set_flag = (r, l1, l2) {
        *(cast(bool*) r.user_data) = true;
    };

    SyntaxReporter.ReportingFunction_loc report_loc_set_flag = (r, l) {
        *(cast(bool*) r.user_data) = true;
    };

    SyntaxReporter.ReportingFunction_token report_bad_token_set_flag = (r, t) {
        *(cast(bool*) r.user_data) = true;
    };

    template parser_init(string s) {
        enum parser_init = "StringTable table; SyntaxReporter error = SyntaxReporter(); auto parser = Parser(\"" ~ s ~ "\", &table, &error);";
    }
}
