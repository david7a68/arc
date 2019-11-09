module arc.syntax.tests.lexer;

import arc.syntax.lexer: Token, Lexer, scan_type;
import arc.stringtable: StringTable;
import arc.syntax.location: SpannedText;

string init_scan(string test_string) {
    import std.format: format;

    return "
        const test_string = \"%s\";
        const(char)* start = test_string.ptr;
        const(char)* cursor = start;
        const char* end = cursor + test_string.length;
    ".format(test_string);
}

unittest {
    StringTable t;
    auto l = Lexer(SpannedText(0, 4, "a\r\n\n"), &t);
    l.push_eol_type(Token.Comma);
    l.ready();
    assert(l.current.type == Token.Name);
    l.advance();
    assert(l.current.type == Token.Comma);
    l.advance();
    assert(l.current.type == Token.Eof);
}

unittest { // Test empty lexer
    mixin(init_scan(""));
    assert(scan_type(cursor, end).type == Token.Eof);
}

unittest { // Test empty lexer with whitespace
    mixin(init_scan("  \t\t\t\t    "));
    assert(scan_type(cursor, end).type == Token.Eof);
}

unittest {
    mixin(init_scan("()[],.;->129400_81anb_wo283'some_label"));

    bool test_scan(Token.Type t, const char[] text, size_t cursor_pos) {
        auto tok = scan_type(cursor, end);
        return (tok.type == t) && (tok.text == text) && (cursor == start + cursor_pos);
    }

    assert(test_scan(Token.Lparen,      "(", 1));
    assert(test_scan(Token.Rparen,      ")", 2));
    assert(test_scan(Token.Lbracket,    "[", 3));
    assert(test_scan(Token.Rbracket,    "]", 4));
    assert(test_scan(Token.Comma,       ",", 5));
    assert(test_scan(Token.Dot,         ".", 6));
    assert(test_scan(Token.Semicolon,   ";", 7));
    assert(test_scan(Token.Rarrow,      "->", 9));
    assert(test_scan(Token.Integer,     "129400_81", 18));
    assert(test_scan(Token.Name,        "anb_wo283", 27));
    assert(test_scan(Token.Label,       "'some_label", 38));

    // Keywords are not preocessed at scan_type level
    assert(scan_type(cursor, end).type == Token.Eof);
}

unittest {
    mixin(init_scan("{
} else break
//    return false"));
    // this may cause an assertion error
    while (scan_type(cursor, end).type != Token.Eof) continue;
}
