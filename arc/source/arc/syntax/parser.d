module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.syntax_reporter;

struct Parser {
    Token token;
    Lexer lexer;

    void reset(const(char)[] source) {
        lexer.reset(source);
        advance();
    }

    void advance() {
        lexer.lex();
        token = lexer.current;
    }

    bool consume(Token.Type type) {
        if (type != token.type)
            return false;
        advance();
        return true;
    }
}

Expression expression(ref Parser p, ref SyntaxReporter error) {
    return expression(p, error, Infix.Precedence.Assignment);
}

unittest {
    Parser parser;

    // token_cannot_start_expr_impl
    bool bad_token;
    auto err = SyntaxReporter(0, &bad_token);
    err.token_cannot_start_expr_impl = report_bad_token_set_flag;
    parser.reset("]");
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
        Call
    }

    Precedence precedence;

    alias InfixParselet = Expression function(ref Parser, ref SyntaxReporter, Expression);
    InfixParselet parselet;
    
    alias Precedence this;
}

immutable Infix[256] infix_parslets = () {
    Infix[256] p;

    p[Token.Plus]       = Infix(Infix.Sum, &add);
    p[Token.Minus]      = Infix(Infix.Sum, &subtract);
    p[Token.Star]       = Infix(Infix.Product, &multiply);
    p[Token.Slash]      = Infix(Infix.Product, &divide);
    p[Token.Caret]      = Infix(Infix.Power, &power);

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
    scope(exit) p.advance();
    return new Integer(p.token.start, p.token.span);
}

Expression list(ref Parser p, ref SyntaxReporter error) {
    const start = p.token.start;
    const closing_tok = p.token.type == Token.Lbracket ? Token.Rbracket : Token.Rparen;
    p.advance();

    // skip leading commas
    while(p.token.type == Token.Comma)
        p.advance();

    AstNode[] members;
    bool subexpression_error = false;
    while (p.token.type != closing_tok) {
        auto e = p.expression(error);

        if (e.type == AstNode.Invalid)
            subexpression_error = true;

        members ~= e;

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
        auto lst = new List(start, (close.start + close.span) - start);
        lst.children = members;
        return lst;
    }
    else {
        auto inv = new Invalid(start, (close.start + close.span) - start);
        return inv;
    }
}

unittest {
    Parser parser;
    auto err = SyntaxReporter();
    parser.reset("[]");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.List);
    assert(e.children.length == 0);
}

unittest {
    Parser parser;
    auto err = SyntaxReporter();
    parser.reset("[a, (b)]");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.List);
    assert(e.children.length == 2);
    assert(e.children[0].type == AstNode.Name);
    assert(e.children[1].type == AstNode.List);
}

unittest {
    Parser parser;
    
    // list not closed, incorrect closing delimiter
    bool not_closed;
    auto err = SyntaxReporter(0, &not_closed);
    err.list_not_closed_impl = report_loc_err_set_flag;
    parser.reset("[a)");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Invalid);
    assert(not_closed);
}

unittest {
    Parser parser;

    // list not closed, unexpected end of file
    bool not_closed;
    auto err = SyntaxReporter(0, &not_closed);
    err.list_not_closed_impl = report_loc_err_set_flag;
    parser.reset("[a, [b)]");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Invalid);
    assert(not_closed);
}

Expression negate(ref Parser p, ref SyntaxReporter error) {
    const start = p.token.start;
    p.consume(Token.Minus);

    auto expr = p.expression(error);
    if (expr.type != AstNode.Invalid)
        return new Negate(expr);

    return new Invalid(start, (expr.start + expr.span) - start);
}

Expression add(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    p.consume(Token.Plus);

    auto rhs = p.expression(error, cast(Infix.Precedence)(Infix.Sum + 1));
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Add(lhs, rhs);

    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

Expression subtract(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    p.consume(Token.Minus);

    auto rhs = p.expression(error, cast(Infix.Precedence)(Infix.Sum + 1));
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Subtract(lhs, rhs);

    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

Expression multiply(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    p.consume(Token.Star);

    auto rhs = p.expression(error, cast(Infix.Precedence)(Infix.Product + 1));
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Multiply(lhs, rhs);

    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

Expression divide(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    p.consume(Token.Slash);

    auto rhs = p.expression(error, cast(Infix.Precedence)(Infix.Product + 1));
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Divide(lhs, rhs);

    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

Expression power(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    p.consume(Token.Caret);

    // we don't do (Infix.Power + 1) because we want '<' in expression() instead
    // of '<='
    auto rhs = p.expression(error, Infix.Power);
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Power(lhs, rhs);

    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

version(unittest) {
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
