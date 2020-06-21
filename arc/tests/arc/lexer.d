module tests.arc.lexer;

import arc.data.span: Span;
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
    for (; tokens[length].type != Token.Type.Done && length < tokens.length; length++) {}

    static if (compare_type)
        return tokens[0 .. length].map!(a => a.type).equal(ts);
    else
        return tokens[0 .. length].equal(ts);
}

@("Lex Empty") unittest {
    assert("".scan_tokens[0].type == Token.Type.Done);
}

@("Lex Whitespace") unittest {
    assert("  \t\t\t\t    ".scan_tokens[0].type == Token.Type.Done);
}

@("Lex Line Comment") unittest {

    assert("#blah blah blah".scan_tokens[0].type == Token.Type.Done);
    assert("#blah\n#blah".scan_tokens[0].type == Token.Type.Done);

    // import std.stdio; writeln("#asflj".scan_tokens[0].span);
}

@("Lex Compact") unittest {
    assert("()[]{},.;->1a_3".scan_tokens.equivalent(
        Token.Type.Lparen,
        Token.Type.Rparen,
        Token.Type.Lbracket,
        Token.Type.Rbracket,
        Token.Type.Lbrace,
        Token.Type.Rbrace,
        Token.Type.Comma,
        Token.Type.Dot,
        Token.Type.Semicolon,
        Token.Type.Rarrow,
        Token.Type.Integer,
        Token.Type.Name,
    ));
}

@("Lex Operators") unittest {
    assert("+ - * / ^ & < > = ! <= != >=".scan_tokens.equivalent(
        Token.Type.Plus,
        Token.Type.Minus,
        Token.Type.Star,
        Token.Type.Slash,
        Token.Type.Caret,
        Token.Type.Ampersand,
        Token.Type.Less,
        Token.Type.Greater,
        Token.Type.Equals,
        Token.Type.Bang,
        Token.Type.LessEqual,
        Token.Type.BangEqual,
        Token.Type.GreaterEqual
    ));
}

@("Lex Keywords") unittest {
    assert("and or if else loop break return continue def".scan_tokens.equivalent!false(
        Token(Token.Type.And,        Span(10, 3), 17648556366517412293UL),
        Token(Token.Type.Or,         Span(14, 2), 4116612837551264357UL),
        Token(Token.Type.If,         Span(17, 2), 10245967140023949179UL),
        Token(Token.Type.Else,       Span(20, 4), 15206279584842378246UL),
        Token(Token.Type.Loop,       Span(25, 4), 14974296207267515915UL),
        Token(Token.Type.Break,      Span(30, 5), 587566040553785743UL),
        Token(Token.Type.Return,     Span(36, 6), 17660342674565394361UL),
        Token(Token.Type.Continue,   Span(43, 8), 16371828708454923059UL),
        Token(Token.Type.Def,        Span(52, 3), 9791288970560527259UL)
    ));
}

@("Lex Char") unittest {
    assert("'a'".scan_tokens.equivalent(Token.Type.Char));
    assert("'\\a'".scan_tokens.equivalent!false(
        Token(Token.Type.Char, Span(10, 4), digest("\\a"))
    ));
}

@("Lex String") unittest {
    assert(`"hello world"`.scan_tokens.equivalent(Token.Type.String));
    assert(`"\""`.scan_tokens.equivalent(Token.Type.String));
}

@("Lex Integers") unittest {
    assert("20".scan_tokens()[0].value == 20);
    assert("1_____23__4___".scan_tokens()[0].value == 1234);
    assert("3b".scan_tokens.equivalent(Token.Type.Integer, Token.Type.Name));
}
