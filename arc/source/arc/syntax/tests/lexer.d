module arc.syntax.tests.lexer;

import arc.syntax.lexer: Lexer, Token;
import arc.syntax.location: Span, SpannedText;

Lexer scan_tokens(const(char)[] text, Token.Type delimiter = Token.Invalid) {
    auto l = Lexer(SpannedText(0, cast(uint) text.length, text));
    l.push_eol_delimiter(delimiter);
    return l;
}

bool seq_equivalent(string expr, T)(Lexer lexer, T[] ts...) {
    while (lexer.current.type != Token.Done) {
        if (ts.length == 0)
            return false;
        mixin("if (" ~ expr ~ " != ts[0]) return false;");
        ts = ts[1 .. $];
        lexer.advance();
    }

    if (ts.length > 0 || !lexer.empty)
        return false;
    return true;
}

alias type_equivalent = seq_equivalent!("lexer.current.type", Token.Type);
alias token_equivalent = seq_equivalent!("lexer.current", Token);

@("lexer:empty") unittest {
    assert("".scan_tokens.empty);
}

@("lexer:whitespace") unittest {
    assert("  \t\t\t\t    ".scan_tokens.empty);
}

@("lexer:compact") unittest {
    assert("()[]{},.;->1a_3'a".scan_tokens.type_equivalent(
        Token.Lparen,
        Token.Rparen,
        Token.Lbracket,
        Token.Rbracket,
        Token.Lbrace,
        Token.Rbrace,
        Token.Comma,
        Token.Dot,
        Token.Semicolon,
        Token.Rarrow,
        Token.Integer,
        Token.Name,
        Token.Label,
    ));
}

@("lexer:keywords") unittest {
    assert("and or if else loop break return continue def".scan_tokens.token_equivalent(
        Token(Token.And,        Span(0, 3), 17648556366517412293UL),
        Token(Token.Or,         Span(4, 2), 4116612837551264357UL),
        Token(Token.If,         Span(7, 2), 10245967140023949179UL),
        Token(Token.Else,       Span(10, 4), 15206279584842378246UL),
        Token(Token.Loop,       Span(15, 4), 14974296207267515915UL),
        Token(Token.Break,      Span(20, 5), 587566040553785743UL),
        Token(Token.Return,     Span(26, 6), 17660342674565394361UL),
        Token(Token.Continue,   Span(33, 8), 16371828708454923059UL),
        Token(Token.Def,        Span(42, 3), 9791288970560527259UL)
    ));
}

@("lexer:labels") unittest {
    assert("'a".scan_tokens.type_equivalent(Token.Label));
    assert("'hi".scan_tokens.type_equivalent(Token.Label));
}

@("lexer:char") unittest {
    assert("'\\a'".scan_tokens.token_equivalent(
        Token(Token.Char, Span(0, 4))
    ));
}

@("lexer:dedup_comma") unittest {
    assert(",,,".scan_tokens.type_equivalent(Token.Comma));
}

@("lexer:dedup_semicolon") unittest {
    assert(";;;".scan_tokens.type_equivalent(Token.Semicolon));
}

@("lexer:insert_delim") unittest {
    static test_tokens(string[] str...) {
        foreach (s; str) {
            auto lexer = s.scan_tokens(Token.Comma);
            lexer.advance();
            assert(lexer.type_equivalent(Token.Comma), s);
        }
    }

    test_tokens(")", "]", "}", "a", "1", "'a", "'a'", "break", "return", "continue");
}

@("lexer:keyword_delim_error") unittest {
    with (Token.Type)
    assert("{return}".scan_tokens(Semicolon).type_equivalent(Lbrace, Return, Semicolon, Rbrace, Semicolon));
}