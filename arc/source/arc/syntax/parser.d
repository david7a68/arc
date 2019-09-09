module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.syntax_reporter;

struct Parser {
    SyntaxReporter error;
    Token token;
    Lexer lexer;

    Expression expression(const(char)[] source, SyntaxReporter reporter) {
        lexer.reset(source);
        error = reporter;

        advance();
        return expression();
    }

    Expression expression() {
        return primary(this, error);
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

private:

Expression primary(ref Parser p, ref SyntaxReporter error) {
    switch (p.token.type) {
    case Token.Lparen:
        return tuple(p, error);
    case Token.Lbracket:
        return vector(p, error);
    case Token.Name:
        return name(p);
    case Token.Integer:
        return integer(p);
    default:
        error.token_cannot_start_expr(p.token);
        return null; // unreachable except in testing
    }
}

unittest {
    Parser parser;

    // token_cannot_start_expr_impl
    bool bad_token;
    auto err = SyntaxReporter(0, &bad_token);
    err.token_cannot_start_expr_impl = report_bad_token_set_flag;
    auto e = parser.expression("]", err);
    assert(bad_token);
}

Expression vector(ref Parser p, ref SyntaxReporter error) {
    const start = p.token.start;
    p.advance();

    // skip leading commas
    while(p.token.type == Token.Comma)
        p.advance();

    auto node = new Vector;
    while (p.token.type != Token.Rbracket) {
        node.add_member(p.expression());

        if (p.token.type == Token.Comma) {
            do {
                p.advance();
            } while (p.token.type == Token.Comma);
        }
        else if (p.token.type == Token.Eof) {
            error.unexpected_end_of_file(p.token.start);
            return new Invalid(start, p.token.start - start);
        }
        else {
            error.vector_missing_comma(start, p.token.start);
            return new Invalid(start, p.token.start - start);
        }
    }

    const close = p.token;
    p.consume(Token.Rbracket);

    node.start = start;
    node.span = (close.start + close.span) - start;
    return node;
}

unittest {
    Parser parser;
    
    // vector_missing_comma
    bool missing_comma;
    auto err = SyntaxReporter(0, &missing_comma);
    err.vector_missing_comma_impl = report_loc_err_set_flag;
    auto e = parser.expression("[a)", err);
    assert(missing_comma);
}

unittest {
    Parser parser;

    // vector_unexpected_eof
    bool unexpected_eof;
    auto err = SyntaxReporter(0, &unexpected_eof);
    err.unexpected_end_of_file_impl = report_loc_set_flag;
    auto e = parser.expression("[a", err);
    assert(unexpected_eof);
}

Expression tuple(ref Parser p, ref SyntaxReporter error) {
    const start = p.token.start;
    p.advance();

    // skip leading commas
    while(p.token.type == Token.Comma)
        p.advance();

    auto node = new Tuple;
    while (p.token.type != Token.Rparen) {
        node.add_member(p.expression());

        if (p.token.type == Token.Comma) {
            do {
                p.advance();
            } while (p.token.type == Token.Comma);
        }
        else if (p.token.type == Token.Eof) {
            error.unexpected_end_of_file(p.token.start);
            return new Invalid(start, p.token.start - start);
        }
        else {
            error.tuple_missing_comma(start, p.token.start);
            return new Invalid(start, p.token.start - start);
        }
    }

    const close = p.token;
    p.consume(Token.Rparen);

    node.start = start;
    node.span = (close.start + close.span) - start;
    return node;
}

unittest {
    Parser parser;
    
    // tuple_missing_comma
    bool missing_comma;
    auto err = SyntaxReporter(0, &missing_comma);
    err.tuple_missing_comma_impl = report_loc_err_set_flag;
    auto e = parser.expression("(a]", err);
    assert(missing_comma);
}

unittest {
    Parser parser;

    // tuple_unexpected_eof
    bool unexpected_eof;
    auto err = SyntaxReporter(0, &unexpected_eof);
    err.unexpected_end_of_file_impl = report_loc_set_flag;
    auto e = parser.expression("(a", err);
    assert(unexpected_eof);
}

Name name(ref Parser p) {
    scope(exit) p.advance();
    return new Name(p.token.start, p.token.span);
}

Integer integer(ref Parser p) {
    scope(exit) p.advance();
    return new Integer(p.token.start, p.token.span);
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
