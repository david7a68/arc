module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.syntax_reporter;

struct Parser {
    import arc.stringtable;

    Token token;
    Lexer lexer;
    StringTable *table;

    void reset(const(char)[] source, StringTable* table) {
        lexer.reset(source, table);
        lexer.ready();
        token = lexer.current;
    }

    void advance() {
        lexer.advance();
        token = lexer.current;
        // import std.stdio; writeln(token);
    }

    bool consume(Token.Type type) {
        if (type != token.type)
            return false;
        
        advance();
        return true;
    }

    void push_eol_type(Token.Type type) {
        lexer.push_eol_type(type);
    }

    void pop_eol_type() {
        lexer.pop_eol_type();
    }
}

Expression expression(ref Parser p, ref SyntaxReporter error) {
    return expression(p, error, Infix.Precedence.Assignment);
}

unittest {
    Parser parser;
    StringTable table;
    auto err = SyntaxReporter(0);

    bool bad_token;
    err.user_data = &bad_token;
    err.token_cannot_start_expr_impl = report_bad_token_set_flag;
    parser.reset("]", &table);
    auto e = parser.expression(err);
    assert(bad_token);
}

private:

alias Prefix = Expression function(ref Parser, ref SyntaxReporter);

immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p;

    p[Token.Lparen] = &list;
    p[Token.Lbracket] = &list;
    p[Token.Name] = &name;
    p[Token.Integer] = &integer;
    p[Token.Minus] = &negate;

    return p;
} ();


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

    alias InfixParselet = Expression function(ref Parser, ref SyntaxReporter, Expression);
    InfixParselet parselet;
    
    alias Precedence this;
}

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

Expression expression(ref Parser p, ref SyntaxReporter error, Infix.Precedence prec) {
    auto parselet = prefix_parslets[p.token.type];

    if (parselet is null) {
        error.token_cannot_start_expr(p.token);
        return new Invalid(p.token.start, p.token.span);
    }
    
    auto subexpression_error = false;
    auto expr = parselet(p, error);
    while (prec <= infix_parslets[p.token.type].precedence) {
        auto infix = infix_parslets[p.token.type].parselet;

        if (infix !is null) {
            expr = infix(p, error, expr);
        }
        else {
            error.token_is_not_an_operator(expr.start, p.token);
            
            // skip the token, and then the rest of the expression
            p.advance();
            auto _rhs = p.expression(error);

            expr = new Invalid(expr.start, (_rhs.start + _rhs.span) - expr.start);
            subexpression_error = true;
        }
    }

    if (!subexpression_error)
        return expr;
    
    return new Invalid(expr.start, expr.span);
}

Name name(ref Parser p, ref SyntaxReporter error) {
    scope(exit) p.advance();
    return new Name(p.token.start, p.token.span);
}

Integer integer(ref Parser p, ref SyntaxReporter error) {
    import std.conv: to;

    scope(exit) p.advance();
    return new Integer(p.token.start, p.token.span, p.token.start[0..p.token.span].to!ulong);
}

unittest {
    Parser parser;
    auto err = SyntaxReporter();

    parser.reset("10294", null);
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Integer);
    assert((cast(Integer) e).value == 10_294);
}

Expression list(ref Parser p, ref SyntaxReporter error) {
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
        auto e = p.var_expr(error);

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
            error.list_not_closed(start, p.token.start);
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
    Parser parser;
    StringTable table;
    auto err = SyntaxReporter();

    {
        parser.reset("[]", null);
        auto e = parser.expression(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.List);
        assert(e.children.length == 0);
    }
    {
        parser.reset("[a\n\n\n(b)]", &table);
        auto e = parser.expression(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.List);
        assert(e.children.length == 2);
        assert(e.children[0].type == AstNode.VarExpression);
        assert(e.children[1].type == AstNode.VarExpression);
    }
    {
        bool[2] errors; // [0]: cannot start expr, [1]: list not closed
        err.user_data = &errors;
        err.token_cannot_start_expr_impl = (self, tok) {
            (cast(bool[2]*) self.user_data)[0] = true;
        };
        err.list_not_closed_impl = (self, loc, erloc) {
            (cast(bool[2]*) self.user_data)[1] = true;
        };

        parser.reset("[a\n)", &table);
        auto e = parser.expression(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.Invalid);
        assert(errors[0] && errors[1]);
    }
    {
        bool not_closed;
        err.user_data = &not_closed;
        err.list_not_closed_impl = report_loc_err_set_flag;

        parser.reset("[a, [b)]", &table);
        auto e = parser.expression(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.Invalid);
        assert(not_closed);
    }
}

Expression negate(ref Parser p, ref SyntaxReporter error) {
    const start = p.token.start;
    p.consume(Token.Minus);

    auto expr = prefix_parslets[p.token.type](p, error);
    if (expr.type != AstNode.Invalid)
        return new Negate(expr);

    return new Invalid(start, (expr.start + expr.span) - start);
}

Expression function_(ref Parser p, ref SyntaxReporter error, Expression params) {
    p.consume(Token.Rarrow);

    auto body = p.expression(error);

    if (params.type != AstNode.Invalid && body.type != AstNode.Invalid)
        return new Function(params, body);

    return new Invalid(params.start, (body.start + body.span) - params.start);
}

unittest {
    Parser parser;
    auto err = SyntaxReporter();

    parser.reset("() -> ()", null);
    auto e = cast(Function) parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.parameters.type == AstNode.List);
    assert(e.body.type == AstNode.List);
}

Expression binary(T, int prec, Token.Type ttype)(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    p.consume(ttype);

    auto rhs = p.expression(error, cast(Infix.Precedence) prec);
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new T(lhs, rhs);
    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

alias add = binary!(Add, Infix.Sum + 1, Token.Plus);
alias subtract = binary!(Subtract, Infix.Sum + 1, Token.Minus);
alias multiply = binary!(Multiply, Infix.Product + 1, Token.Star);
alias divide = binary!(Divide, Infix.Product + 1, Token.Slash);
alias power = binary!(Power, Infix.Power, Token.Caret);

Expression call(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    auto rhs = p.list(error);

    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Call(lhs, rhs);
    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

unittest {
    Parser parser;
    auto err = SyntaxReporter();

    parser.reset("[][()]", null);
    auto e = cast(Call) parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Call);
    assert(e.children.length == 2);
    assert(e.target.type == AstNode.List);
    assert(e.arguments.type == AstNode.List);
    assert((cast(VarExpression) e.arguments.children[0]).value_expr.type == AstNode.List);
}

/**
 * var_expr : Expression ((":" Expression ("=" Expression)?) |
                          ("=" Expression))?
 */
Expression var_expr(ref Parser p, ref SyntaxReporter error) {
    Expression first, type_expr, value_expr;
    const start = p.token.start;
    const(char)* end = start;
    bool subexpression_error;

    first = p.expression(error);
    end = first.start + first.span;
    if (first.type == AstNode.Invalid)
        subexpression_error = true;

    if (p.consume(Token.Colon)) {
        // var_expr : Expression (":" Expression)?
        type_expr = p.expression(error);
        end = type_expr.start + type_expr.span;
        if (type_expr.type == AstNode.Invalid)
            subexpression_error = true;

        if (p.consume(Token.Equals)) {
            // var_expr : Expression (":" Expression ("=" Expression)?)?
            value_expr = p.expression(error);
            end = value_expr.start + value_expr.span;
            if (value_expr.type == AstNode.Invalid)
                subexpression_error = true;
        }
    }
    else if (p.consume(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        value_expr = p.expression(error);
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
    Parser parser;
    StringTable table;
    auto err = SyntaxReporter();

    {
        parser.reset("a", &table);
        auto e = cast(VarExpression) parser.var_expr(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern is null);
        assert(e.type_expr is null);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        parser.reset("a:b", &table);
        auto e = cast(VarExpression) parser.var_expr(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr.type == AstNode.Name);
        assert(e.value_expr is null);
    }
    {
        parser.reset("a:b=c", &table);
        auto e = cast(VarExpression) parser.var_expr(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr.type == AstNode.Name);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        parser.reset("a=c", &table);
        auto e = cast(VarExpression) parser.var_expr(err);
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr is null);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        bool var_error;
        err.user_data = &var_error;
        err.token_cannot_start_expr_impl = report_bad_token_set_flag;
        
        parser.reset("a:=c", &table);
        auto e = parser.var_expr(err);
        assert(parser.token.type == Token.Eof);
        assert(var_error);
    }
}

version(unittest) {
    import arc.stringtable: StringTable;

    SyntaxReporter.ReportingFunction_loc_err report_loc_err_set_flag = (r, l1, l2) {
        *(cast(bool*) r.user_data) = true;
    };

    SyntaxReporter.ReportingFunction_loc report_loc_set_flag = (r, l) {
        *(cast(bool*) r.user_data) = true;
    };

    SyntaxReporter.ReportingFunction_token report_bad_token_set_flag = (r, t) {
        *(cast(bool*) r.user_data) = true;
    };
}
