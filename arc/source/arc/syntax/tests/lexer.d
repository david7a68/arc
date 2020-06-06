module arc.syntax.tests.lexer;

import arc.data.source: Span;
import arc.data.hash: digest;
import arc.syntax.lexer: Token, TokenBuffer;

/// Thread-local global token buffer so we don't have to allocate a whole bunch
/// of these. Also makes the interface of `scan_tokens(text)` simpler.
TokenBuffer!64 token_buffer;

/// Read all tokens from text, up to `token_buffer.length`.
Token[] scan_tokens(const(char)[] text) {
    token_buffer.begin(text, 10);
    return token_buffer.tokens[];
}

/// Tests that the tokens provided are equivalent in type or value to `ts`.
bool equivalent(bool compare_type = true, T)(Token[] tokens, T[] ts...) {
    import std.algorithm: equal, map;
    import std.range: zip;
    import std.format: format;

    size_t length;
    for (; tokens[length].type != Token.Done && length < tokens.length; length++) {}

    static if (compare_type)
        return tokens[0 .. length].map!(a => a.type).equal(ts);
    else
        return tokens[0 .. length].equal(ts);
}

@("Lex Empty") unittest {
    assert("".scan_tokens[0].type == Token.Done);
}

@("Lex Whitespace") unittest {
    assert("  \t\t\t\t    ".scan_tokens[0].type == Token.Done);
}

@("Lex Line Comment") unittest {

    assert("#blah blah blah".scan_tokens[0].type == Token.Done);
    assert("#blah\n#blah".scan_tokens[0].type == Token.Done);

    // import std.stdio; writeln("#asflj".scan_tokens[0].span);
}

@("Lex Compact") unittest {
    assert("()[]{},.;->1a_3".scan_tokens.equivalent(
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
    ));
}

@("Lex Operators") unittest {
    assert("+ - * / ^ & < > = ! <= != >=".scan_tokens.equivalent(
        Token.Plus,
        Token.Minus,
        Token.Star,
        Token.Slash,
        Token.Caret,
        Token.Ampersand,
        Token.Less,
        Token.Greater,
        Token.Equals,
        Token.Bang,
        Token.LessEqual,
        Token.BangEqual,
        Token.GreaterEqual
    ));
}

@("Lex Keywords") unittest {
    assert("and or if else loop break return continue def".scan_tokens.equivalent!false(
        Token(Token.And,        Span(10, 3), 17648556366517412293UL),
        Token(Token.Or,         Span(14, 2), 4116612837551264357UL),
        Token(Token.If,         Span(17, 2), 10245967140023949179UL),
        Token(Token.Else,       Span(20, 4), 15206279584842378246UL),
        Token(Token.Loop,       Span(25, 4), 14974296207267515915UL),
        Token(Token.Break,      Span(30, 5), 587566040553785743UL),
        Token(Token.Return,     Span(36, 6), 17660342674565394361UL),
        Token(Token.Continue,   Span(43, 8), 16371828708454923059UL),
        Token(Token.Def,        Span(52, 3), 9791288970560527259UL)
    ));
}

@("Lex Char") unittest {
    assert("'a'".scan_tokens.equivalent(Token.Char));
    assert("'\\a'".scan_tokens.equivalent!false(
        Token(Token.Char, Span(10, 4), digest("\\a"))
    ));
}

@("Lex Integers") unittest {
    assert("20".scan_tokens()[0].value == 20);
    assert("1_____23__4___".scan_tokens()[0].value == 1234);
    assert("3b".scan_tokens.equivalent(Token.Integer, Token.Name));
}
