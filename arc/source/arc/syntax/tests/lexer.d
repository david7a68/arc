module arc.syntax.tests.lexer;

import arc.data.source: Span;
import arc.syntax.lexer: Token, TokenBuffer;

/// Thread-local global token buffer so we don't have to allocate a whole bunch
/// of these. Also makes the interface of `scan_tokens(text)` simpler.
TokenBuffer!64 token_buffer;

/// Read all tokens from text, up to `token_buffer.length`.
auto scan_tokens(const(char)[] text) {
    token_buffer.begin(text);
    assert(token_buffer.next_buffer_index == text.length, "Exceeded max test length");

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

@("lex empty") unittest {
    assert("".scan_tokens[0].type == Token.Done);
}

@("lex whitespace") unittest {
    assert("  \t\t\t\t    ".scan_tokens[0].type == Token.Done);
}

@("lex compact") unittest {
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

@("lex operators") unittest {
    import arc.data.hash: digest;

    auto tokens = "+ - * / ^ & < > = ! <= != >=".scan_tokens;
    auto strings = ["+", "-", "*", "/", "^", "&", "<", ">", "=", "!", "<=", "!=", ">="];

    import std.range: lockstep;
    foreach (token, text; lockstep(tokens, strings))
        assert(token.key == digest(text));
}

@("lex keywords") unittest {
    assert("and or if else loop break return continue def".scan_tokens.equivalent!false(
        Token(Token.And,        Span(0, 3), 2779594451),
        Token(Token.Or,         Span(4, 2), 117848935),
        Token(Token.If,         Span(7, 2), 491396510),
        Token(Token.Else,       Span(10, 4), 4025523449),
        Token(Token.Loop,       Span(15, 4), 3830345903),
        Token(Token.Break,      Span(20, 5), 1271411175),
        Token(Token.Return,     Span(26, 6), 1430125705),
        Token(Token.Continue,   Span(33, 8), 3783755232),
        Token(Token.Def,        Span(42, 3), 4118380002)
    ));
}

@("lex char") unittest {
    assert("'a'".scan_tokens.equivalent(Token.Char));
    assert("'\\a'".scan_tokens.equivalent!false(
        Token(Token.Char, Span(0, 4), 1676188941)
    ));
}
