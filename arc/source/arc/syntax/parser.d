module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.syntax_reporter;

struct Parser {
    SyntaxReporter error;
    Token token;
    Lexer lexer;

    Expression parse(const(char)[] source, SyntaxReporter reporter) {
        return expression(source, reporter);
    }

    Expression expression(const(char)[] source, SyntaxReporter reporter) {
        lexer.reset(source);
        error = reporter;

        advance();
        return expression();
    }

    Expression expression() {
        return primary();
    }

    Expression primary() {
        switch (token.type) {
        case Token.Lparen:
            return tuple();
        case Token.Lbracket:
            return vector();
        case Token.Name:
            return name();
        case Token.Integer:
            return integer();
        default:
            error.token_cannot_start_expr(token);
            return null; // unreachable except in testing
        }
    }

    Vector vector() {
        const start = token.start;
        advance();

        // skip leading commas
        while(token.type == Token.Comma)
            advance();

        auto node = new Vector;
        while (token.type != Token.Rbracket) {
            node.add_member(expression());

            if (token.type == Token.Comma) {
                do {
                    advance();
                } while (token.type == Token.Comma);
            }
            else if (token.type == Token.Eof) {
                error.unexpected_end_of_file(token.start);
                return null;
            }
            else {
                error.vector_missing_comma(start, token.start);
                return null;
            }
        }

        node.start = start;
        node.span = (token.start + token.span) - start;
        const correct = consume(Token.Rbracket);
        assert(correct);
        return node;
    }

    Expression tuple() {
        const start = token.start;
        advance();

        // skip leading commas
        while(token.type == Token.Comma)
            advance();

        auto node = new Tuple;
        while (token.type != Token.Rparen) {
            node.add_member(expression());

            if (token.type == Token.Comma) {
                do {
                    advance();
                } while (token.type == Token.Comma);
            }
            else if (token.type == Token.Eof) {
                error.unexpected_end_of_file(token.start);
                return new Invalid(start, token.start - start);
            }
            else {
                error.tuple_missing_comma(start, token.start);
                return new Invalid(start, token.start - start);
            }
        }

        node.start = start;
        node.span = (token.start + token.span) - start;
        const correct = consume(Token.Rparen);
        assert(correct);
        return node;
    }

    Name name() {
        scope(exit) advance();
        return new Name(token.start, token.span);
    }

    Integer integer() {
        scope(exit) advance();
        return new Integer(token.start, token.span);
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

version(unittest):

SyntaxReporter.ReportingFunction_loc_err report_loc_err_set_flag = (r, l1, l2) {
    *(cast(bool*) r.user_data) = true;
};

SyntaxReporter.ReportingFunction_loc report_loc_set_flag = (r, l) {
    *(cast(bool*) r.user_data) = true;
};

SyntaxReporter.ReportingFunction_token report_bad_token_set_flag = (r, t) {
    *(cast(bool*) r.user_data) = true;
};

unittest {
    Parser parser;

    // token_cannot_start_expr_impl
    bool bad_token;
    auto err = SyntaxReporter(0, &bad_token);
    err.token_cannot_start_expr_impl = report_bad_token_set_flag;
    auto e = parser.expression("]", err);
    assert(bad_token);
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
