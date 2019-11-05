module arc.syntax.lexer;

import arc.stringtable: StringTable;
import arc.syntax.location: SpannedText;
import arc.hash: Key;


/**
 * A Token is the smallest discrete unit that represents useful information in
 * an Arc program.
 *
 * Note that this token representation does not know where it came from. That is
 * the responsibility of higher-level modules to keep track of.
 */
struct Token {
    /**
     * Every token has a type, which informs the parser how it needs to process
     * the token into a useful representation of an analyzed piece of code
     * called an abstract syntax tree (AST).
     */
    enum Type: ubyte {
        Invalid, Eof, Eol = '\n',
        Name, Integer, Char,

        Lparen = '(', Rparen = ')',
        Lbracket = '[', Rbracket = ']',
        Lbrace = '{', Rbrace = '}',
        Comma = ',', Dot = '.', Semicolon = ';', Colon = ':',
        Ampersand = '&',
        Plus = '+', Minus = '-', Slash = '/', Star = '*', Caret = '^',
        Equals = '=',
        Less = '<', Greater = '>',
        LessEqual, GreaterEqual, EqualEqual, BangEqual,
        Rarrow, ColonColon, Label, DotDot,

        And, Or,
        If, Else, Loop, Break, Return, Continue,
        Def,
    }

    alias Type this;

    /// The type classification of the token
    Type type;
    
    ///
    SpannedText span;
    
    /// The unique key that identifies a string. This member is unused if the
    /// token is not an name
    Key key;
}


/// Hashmap of reserved keywords and their corresponding token types
immutable Token.Type[Key] keywords;

shared static this() {
    keywords[StringTable.digest("and")] = Token.And;
    keywords[StringTable.digest("or")] = Token.Or;
    keywords[StringTable.digest("if")] = Token.If;
    keywords[StringTable.digest("else")] = Token.Else;
    keywords[StringTable.digest("loop")] = Token.Loop;
    keywords[StringTable.digest("break")] = Token.Break;
    keywords[StringTable.digest("return")] = Token.Return;
    keywords[StringTable.digest("continue")] = Token.Continue;
    keywords[StringTable.digest("def")] = Token.Def;
}


/**
 * Input: Contiguous memory region containing arbitrary data.
 * Output: Sequence of classified sections of the arbitrary data.
 */
struct Lexer {
    import std.container.array: Array;

    /// A pointer to the current character in the source text
    const(char)* source_text;
    /// A pointer to the end of the source text
    const(char)* end_of_text;
    ///
    StringTable* table;
    ///
    SpannedText source;

    /// The ID with which the source text may be referred to
    /// The most recently analyzed token
    Token current;

    /**
     * Stores a stack of delimiters that will be automatically inserted after
     * any of: ) ] <NAME> <INTEGER>
     */
    Array!(Token.Type) eol_type_stack;

    this(SpannedText source, StringTable* table) {
        source_text = source.text.ptr;
        end_of_text = source.text.ptr + source.length;
        this.table = table;
        this.source = source;

        eol_type_stack.clear();
    }

    /// Advances the lexer so that `current` is the first-read token.
    void ready() {
        advance();
    }

    /// Push a token onto the automatic-delimiter-insertion stack
    void push_eol_type(Token.Type t) {
        eol_type_stack.insertBack(t);
    }

    /// Pop a token from the automatic-delimiter-insertion stack
    void pop_eol_type() {
        eol_type_stack.removeBack();
    }

    Token.Type eol_type() {
        return eol_type_stack.length > 0 ? eol_type_stack.back : Token.Invalid;
    }

    Token take() {
        scope(exit) advance();
        return current;
    }

    bool matches_one(Token.Type[] t...) {
        foreach (type; t)
            if (type == current.type) return true;
        return false;
    }

    bool skip(Token.Type t) {
        if (current.type == t) {
            advance();
            return true;
        }
        return false;
    }

    void skip_all(Token.Type t) {
        while (skip(t)) continue;
    }

    /// Scan a new token, automatically inserting end-of-line tokens as needed
    void advance() {
        auto scan = scan_token(source_text, end_of_text, current.type, eol_type());

        Key key;
        if (scan.type == Token.Name) {
            key = table.insert(scan.text);
            scan.type = keywords.get(key, Token.Name);
        }
        else if (scan.type == Token.Label || scan.type == Token.Char) {
            key = table.insert(scan.text);
        }

        current = Token(scan.type, source.get_span(scan.text), key);
    }
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

struct ScanResult { Token.Type type; const(char)[] text; }

ScanResult scan_token(ref const(char)* cursor, const char* end, Token.Type previous, Token.Type eol_type) {
    const start = cursor;
    auto scan = scan_type(cursor, end);

    bool is_rbrace;
    if (scan.type == Token.Rbrace) {
        // rewind the scanner so that we don't replace the Rbrace with eol_type
        cursor = start;
        is_rbrace = true;
    }

    if ((is_rbrace || scan.type == Token.Eol || scan.type == Token.Eof) && eol_type != Token.Invalid) {
        switch (previous) with (Token.Type) {
            case Rparen:
            case Rbracket:
            case Rbrace:
            case Name:
            case Integer:
            case Label:
            case Char:
            case Break:
            case Continue:
            case Return:
                return ScanResult(eol_type, scan.text);
            default:
                do {
                    scan = scan_type(cursor, end);
                } while (scan.type == Token.Eol);
        }
    }
    return scan;
}

/**
 * Scans a character buffer from `cursor` to `end`, producing a tuple of the 
 * type of the first token, and its length. The scanner does not distinguish
 * between tokens of the same character class, such as keywords and identifiers,
 * leaving that up to a higher-level scanner.
 * 
 * Returns: (type: Token.Type, text: const(char)[])
 */
ScanResult scan_type(ref const(char)* cursor, const char* end) {
    auto start = cursor;

    auto make_token(Token.Type t, int advance_n) {
        cursor += advance_n;
        assert(cursor <= end);
        return ScanResult(t, start[0 .. cursor - start]);
    }

    switch_start:
    if (cursor >= end)
        return ScanResult(Token.Eof, (end - 1)[0 .. 0]); 

    // @Optimize A character-table driven approach may be more performant.
    switch (*cursor) {
        case ' ':
        case '\t':
        case '\r':
            cursor++;
            start++;
            goto switch_start;
        case '\n':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case ',':
        case ';':
        case '+':
        case '*':
        case '^':
        case '&':
            return make_token(cast(Token.Type) *cursor, 1);
        case '-':
            cursor++;
            if (cursor != end && *cursor == '>')
                return make_token(Token.Rarrow, 1);
            return make_token(Token.Minus, 0);
        case '.':
            cursor++;
            if (cursor <= end && *cursor == '.')
                return make_token(Token.DotDot, 1);
            return make_token(Token.Dot, 0);
        case ':':
            cursor++;
            if (cursor <= end && *cursor == ':')
                return make_token(Token.ColonColon, 1);
            else
                return make_token(Token.Colon, 0);
        case '/':
            cursor++;
            if (cursor <= end && *cursor == '/') {
                while (*cursor != '\n') cursor++;
                goto switch_start;
            }
            else return make_token(Token.Slash, 0);
        case '=':
            cursor++;
            if (cursor != end && *cursor == '=')
                return make_token(Token.EqualEqual, 1);
            else // skip advancing here because we've already done it
                return make_token(Token.Equals, 0);
        case '<':
            cursor++;
            if (cursor != end && *cursor == '=')
                return make_token(Token.LessEqual, 1);
            else
                return make_token(Token.Less, 0);
        case '>':
            cursor++;
            if (cursor != end && *cursor == '=')
                return make_token(Token.GreaterEqual, 1);
            else
                return make_token(Token.Greater, 0);
        case '!':
            cursor++;
            if (cursor != end && *cursor == '=')
                return make_token(Token.BangEqual, 1);
            else
                return make_token(Token.Invalid, 0);
        case '\'':
            cursor++;
            if (cursor == end)
                return make_token(Token.Invalid, 0);
            else if (*cursor == '\\')
                return make_token(Token.Char, 2 + 1);
            else {
                cursor++;

                if (cursor == end)
                    return make_token(Token.Invalid, 0);
                else if (*cursor == '\'')
                    return make_token(Token.Char, 1);
                else {
                    char_loop: if (cursor < end) switch (*cursor) {
                        case 'a': .. case 'z':
                        case 'A': .. case 'Z':
                        case '0': .. case '9':
                        case '_':
                            cursor++;
                            goto char_loop;
                        default:
                    }

                    return make_token(Token.Label, 0);
                }
            }
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
            cursor++;
            loop_start: if (cursor < end) switch (*cursor) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    cursor++;
                    goto loop_start;
                default:
            }
            return make_token(Token.Name, 0);
        case '0': .. case '9':
            cursor++;
            while (('0' <= *cursor && *cursor <= '9') || *cursor == '_')
                cursor++;
            return make_token(Token.Integer, 0);
        default:
            return make_token(Token.Invalid, 1);
    }
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

version(unittest) {
    string init_scan(string test_string) {
        import std.format: format;

        return "
            const test_string = \"%s\";
            const(char)* start = test_string.ptr;
            const(char)* cursor = start;
            const char* end = cursor + test_string.length;
        ".format(test_string);
    }
}
