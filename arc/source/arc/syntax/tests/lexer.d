module arc.syntax.tests.lexer;

import arc.syntax.lexer: Lexer, Token;
import arc.syntax.location: Span, SpannedText;

Lexer scan_tokens(const(char)[] text) {
    auto l = Lexer(SpannedText(0, cast(uint) text.length, text));
    l.advance();
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

    if (ts.length > 0)
        return false;
    assert(lexer.empty);
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
    assert("()[],.;->1a_3'a".scan_tokens.token_equivalent(
        Token(Token.Lparen,     Span(0, 1)),
        Token(Token.Rparen,     Span(1, 1)),
        Token(Token.Lbracket,   Span(2, 1)),
        Token(Token.Rbracket,   Span(3, 1)),
        Token(Token.Comma,      Span(4, 1)),
        Token(Token.Dot,        Span(5, 1)),
        Token(Token.Semicolon,  Span(6, 1)),
        Token(Token.Rarrow,     Span(7, 2)),
        Token(Token.Integer,    Span(9, 1)),
        Token(Token.Name,       Span(10, 3)),
        Token(Token.Label,      Span(13, 2)),
    ));
}

@("lexer:keywords") unittest {
    assert("and or if else loop break return continue def".scan_tokens.token_equivalent(
        Token(Token.And,        Span(0, 3)),
        Token(Token.Or,         Span(4, 2)),
        Token(Token.If,         Span(7, 2)),
        Token(Token.Else,       Span(10, 4)),
        Token(Token.Loop,       Span(15, 4)),
        Token(Token.Break,      Span(20, 5)),
        Token(Token.Return,     Span(26, 6)),
        Token(Token.Continue,   Span(33, 8)),
        Token(Token.Def,        Span(42, 3)),
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
            auto lexer = s.scan_tokens;
            lexer.push_eol_delimiter(Token.Comma);
            lexer.drop();
            assert(lexer.type_equivalent(Token.Comma), s);
        }
    }

    test_tokens(")", "]", "}", "a", "1", "'a", "'a'", "break", "return", "continue");
}

@("lexer:insert_keyword_delim") unittest {
    // static test_tokens(string[] str...) {
    //     foreach (s; str) {
    //         auto lexer = ("a " ~ s).scan_tokens;
    //         lexer.push_eol_delimiter(Token.Semicolon);
    //         import std.stdio;
    //         for (Token t = lexer.current; !lexer.empty; lexer.advance)
    //             write(t, " ");
    //         writeln();
    //         // lexer.drop();
    //         // assert(lexer.type_equivalent(Token.Semicolon), s);
    //     }
    // }

    // test_tokens("}", "if", "else", "break", "continue", "return", "loop");

    auto lexer = "a }".scan_tokens;
    // lexer.push_eol_delimiter(Token.Semicolon);
    
    
    import std.stdio;
    for (Token t = lexer.current; !lexer.empty; lexer.advance())
        write(t.type, " ");
    writeln();
    
    // assert(lexer.type_equivalent(Token.Semicolon, Token.Rbrace));
}
