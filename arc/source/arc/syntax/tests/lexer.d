module arc.syntax.tests.lexer;

import arc.syntax.lexer: Cursor, Token, scan_token, scan_type;
import arc.source: Span;
import arc.stringtable: StringTable;

struct Lexer {
    Cursor cursor;
    Token.Type delimiter;
    StringTable strings;

    Token front;
    bool empty() { return front.type == Token.Done; }
    void popFront() { front = scan_token(cursor, front.type, delimiter, strings); }
}

auto scan_tokens(const(char)[] text, Token.Type delimiter = Token.Invalid) {
    auto l = Lexer(Cursor(text), delimiter, new StringTable);
    l.popFront();
    return l;
}

bool seq_equivalent(string expr, T)(Lexer lexer, T[] ts...) {
    while (lexer.front.type != Token.Done) {
        if (ts.length == 0)
            return false;
        mixin("const eq = " ~ expr ~ " == ts[0];");
        if (!eq) {
            import std.stdio: writefln;
            writefln("Tokens not equal: %s and %s", lexer.front.type, ts[0]);
            return false;
        }

        ts = ts[1 .. $];
        lexer.popFront();
    }
    
    return ts.length == 0 && lexer.empty;
}

alias type_equivalent = seq_equivalent!("lexer.front.type", Token.Type);
alias token_equivalent = seq_equivalent!("lexer.front", Token);

@("lex empty") unittest {
    assert("".scan_tokens.empty);
}

@("lex whitespace") unittest {
    assert("  \t\t\t\t    ".scan_tokens.empty);
}

@("lex compact") unittest {
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

@("lex keywords") unittest {
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

@("lex labels") unittest {
    assert("'a".scan_tokens.type_equivalent(Token.Label));
    assert("'hi".scan_tokens.type_equivalent(Token.Label));
}

@("lex char") unittest {
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
    static test_tokens(string[] strings...) {
        foreach (s; strings) {
            auto lexer = s.scan_tokens(Token.Comma);
            lexer.popFront();
            const token = lexer.front;
            assert(token.type == Token.Comma);
            assert(token.span == Span(cast(uint) (s.length), 0));
        }
    }

    test_tokens(")", "]", "}", "a", "1", "'a", "'a'", "break", "return", "continue");
}

@("lexer:keyword_delim_error") unittest {
    with (Token.Type)
    assert("{return}".scan_tokens(Semicolon).type_equivalent(Lbrace, Return, Semicolon, Rbrace, Semicolon));
}
