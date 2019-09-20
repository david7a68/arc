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
    return primary(p, error);
}

private:

Expression primary(ref Parser p, ref SyntaxReporter error) {
    switch (p.token.type) {
    case Token.Lparen:
    case Token.Lbracket:
        return list(p, error);
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
    parser.reset("]");
    auto e = parser.expression(err);
    assert(bad_token);
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
    parser.reset("[a]");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.List);
    assert(e.children.length == 1);
    assert(e.children[0].type == AstNode.Name);
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
    parser.reset("[a");
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
