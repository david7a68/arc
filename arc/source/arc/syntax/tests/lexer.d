module arc.syntax.tests.lexer;

import arc.data.source: Span;
import arc.syntax.lexer: Token, read_tokens;

Token[64] token_buffer;

auto scan_tokens(const(char)[] text) {
    auto buffer = read_tokens(text, token_buffer);
    assert(buffer.length > 0 && buffer[$-1].type == Token.Done, "Exceeded max test length");

    return buffer;
}

bool equivalent(bool compare_type = true, T)(Token[] tokens, T[] ts...) {
    import std.algorithm: equal, map;
    import std.range: zip;
    import std.format: format;

    tokens = tokens[0 .. $ - 1];

    static if (compare_type)
        return tokens.map!(a => a.type).equal(ts);
    else
        return tokens.equal(ts);
}

@("lex empty") unittest {
    assert("".scan_tokens.length == 1);
}

@("lex whitespace") unittest {
    assert("  \t\t\t\t    ".scan_tokens.length == 1);
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

@("lex char") unittest {
    assert("'a'".scan_tokens.equivalent(Token.Char));
    assert("'\\a'".scan_tokens.equivalent!false(
        Token(Token.Char, Span(0, 4))
    ));
}
