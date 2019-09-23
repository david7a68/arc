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
    parser.reset("10294");
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
    parser.reset("[a\n\n\n(b)]");
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
    bool[2] errors; // [0]: cannot start expr, [1]: list not closed
    auto err = SyntaxReporter(0, &errors);
    err.token_cannot_start_expr_impl = (self, tok) {
        (cast(bool[2]*) self.user_data)[0] = true;
    };
    err.list_not_closed_impl = (self, loc, erloc) {
        (cast(bool[2]*) self.user_data)[1] = true;
    };
    parser.reset("[a\n)");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Invalid);
    assert(errors[0] && errors[1]);
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

    auto expr = prefix_parslets[p.token.type](p, error);
    if (expr.type != AstNode.Invalid)
        return new Negate(expr);

    return new Invalid(start, (expr.start + expr.span) - start);
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

unittest {
    Parser parser;
    auto err = SyntaxReporter();
    // ((a + b - c) * (d ^ e)) / f
    parser.reset("(a + b - c) * d ^ e / f");
    auto e = parser.expression(err);

    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Divide);
    assert(e.children[0].type == AstNode.Multiply);
    assert(e.children[0].children[0].type == AstNode.List);
    assert(e.children[0].children[0].children[0].type == AstNode.Subtract);
    assert(e.children[0].children[0].children[0].children[0].type == AstNode.Add);
    assert(e.children[0].children[0].children[0].children[0].children[0].text == "a");
    assert(e.children[0].children[0].children[0].children[0].children[1].text == "b");
    assert(e.children[0].children[0].children[0].children[1].text == "c");
    assert(e.children[0].children[1].type == AstNode.Power);
    assert(e.children[0].children[1].children[0].text == "d");
    assert(e.children[0].children[1].children[1].text == "e");
    assert(e.children[1].text == "f");

}

Expression call(ref Parser p, ref SyntaxReporter error, Expression lhs) {
    auto rhs = p.list(error);

    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Call(lhs, rhs);
    return new Invalid(lhs.start, (rhs.start + rhs.span) - lhs.start);
}

unittest {
    Parser parser;
    auto err = SyntaxReporter();
    parser.reset("[][()]");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Call);
    assert(e.children.length == 2);
    assert(e.children[0].type == AstNode.List);
    assert(e.children[1].type == AstNode.List);
    assert(e.children[1].children[0].type == AstNode.List);
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
