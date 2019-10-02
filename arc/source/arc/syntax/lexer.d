module arc.syntax.lexer;

/**
 * A Token is the smallest discrete unit that represents useful information in
 * an Arc program.
 *
 * Note that this token representation does not know where it came from. That is
 * the responsibility of higher-level modules to keep track of.
 */
struct Token {
    import arc.hash: Key;

    /**
     * Every token has a type, which informs the parser how it needs to process
     * the token into a useful representation of an analyzed piece of code
     * called an abstract syntax tree (AST).
     */
    enum Type: ubyte {
        Invalid, Eof, Eol = '\n',
        Name, Integer,

        Lparen = '(', Rparen = ')',
        Lbracket = '[', Rbracket = ']',
        Comma = ',', Dot = '.', Semicolon = ';', Colon = ':',
        Plus = '+', Minus = '-', Slash = '/', Star = '*', Caret = '^',
        Equals = '=',
        Rarrow,

        If, Else, Loop, Break, Return,
        Let, Def,
    }

    alias Type this;

    /// The type classification of the token
    Type type;
    /// A permanent pointer to the first character in this token
    const(char)* start;
    /// The length of the token in bytes
    size_t span;
    /// The unique key that identifies a string. This member is unused if the
    /// token is not an name
    Key key;
}


import arc.hash: Key;
/// Hashmap of reserved keywords and their corresponding token types
immutable Token.Type[Key] keywords;

shared static this() {
    import arc.stringtable: StringTable;

    keywords[StringTable.digest("if")] = Token.If;
    keywords[StringTable.digest("else")] = Token.Else;
    keywords[StringTable.digest("loop")] = Token.Loop;
    keywords[StringTable.digest("break")] = Token.Break;
    keywords[StringTable.digest("return")] = Token.Return;
    keywords[StringTable.digest("let")] = Token.Let;
    keywords[StringTable.digest("def")] = Token.Def;
}


/**
 * Input: Contiguous memory region containing arbitrary data.
 * Output: Sequence of classified sections of the arbitrary data.
 */
struct Lexer {
    import std.container.array: Array;

    import arc.stringtable: StringTable;

    /// A pointer to the current character in the source text
    const(char)* source_text;
    /// A pointer to the end of the source text
    const(char)* end_of_text;
    /// The ID with which the source text may be referred to
    /// The most recently analyzed token
    Token next, current;

    /**
     * Stores a stack of delimiters that will be automatically inserted after
     * any of: ) ] <NAME> <INTEGER>
     */
    Array!(Token.Type) eol_type_stack;

    ///
    StringTable* table;

    this(const(char)[] source, StringTable* table) {
        source_text = source.ptr;
        end_of_text = source.ptr + source.length;
        eol_type_stack.clear();
        this.table = table;
    }

    /// Advances the lexer so that `current` is the first-read token.
    void ready() {
        advance();
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

    /// Scan a new token, automatically inserting end-of-line tokens as needed
    void advance() {
        if (next.type == Token.Eol) {
            switch (current.type) with (Token.Type) {
            case Rparen:
            case Rbracket:
            case Name:
            case Integer:
                current = Token(eol_type_stack.back, next.start, 0);
                break;
            default:
                do {
                    current = next;
                    next = scan_token();
                } while (current.type == Token.Eol);
            }
        }
        else {
            current = next;
            next = scan_token();
        }
    }

    /// Scans a token and nothing else.
    Token scan_token() {
        auto start = source_text;
        auto type = scan_type(start, source_text, end_of_text);
        Key key;
        const length = source_text - start;
        
        if (type == Token.Name) {
            key = table.insert(start[0 .. length]);
            type = keywords.get(key, Token.Name);
        }

        return Token(type, start, length, key);
    }
}

unittest { // Test keyword detection
    import arc.stringtable: StringTable;
    import std.range: zip;

    const keys = [
        "if", "else", "loop", "break", "return", "let", "def"
    ];
    const types = [
        Token.If, Token.Else, Token.Loop, Token.Break, Token.Return, Token.Let, Token.Def
    ];

    StringTable table;
    foreach (pair; zip(keys, types))
        assert(lex(pair[0], &table).scan_token.type == pair[1]);
}


Token.Type scan_type(ref const(char)* start, ref const(char)* cursor, const char* end_of_text) {
    Token.Type char_token(Token.Type t, int advance_n = 1) {
        cursor += advance_n;
        return t;
    }

    switch_start:
    if (cursor >= end_of_text)
        return Token.Eof; 

    // @Optimize A character-table driven approach may be more performant.
    switch (*cursor) {
        case ' ':
        case '\t':
        case '\r':
            cursor++;
            start++;
            goto switch_start;
        case '\n':
            return char_token(Token.Eol);
        case '(':
            return char_token(Token.Lparen);
        case ')':
            return char_token(Token.Rparen);
        case '[':
            return char_token(Token.Lbracket);
        case ']':
            return char_token(Token.Rbracket);
        case ',':
            return char_token(Token.Comma);
        case '.':
            return char_token(Token.Dot);
        case ';':
            return char_token(Token.Semicolon);
        case ':':
            return char_token(Token.Colon);
        case '+':
            return char_token(Token.Plus);
        case '-':
            cursor++;
            if (*cursor == '>') // advance only by one
                return char_token(Token.Rarrow, 1);
            else // skip advancing here because we've already done it
                return char_token(Token.Minus, 0);
        case '*':
            return char_token(Token.Star);
        case '/':
            return char_token(Token.Slash);
        case '^':
            return char_token(Token.Caret);
        case '=':
            return char_token(Token.Equals);
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
            cursor++;
            name_start:
            switch (*cursor) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    cursor++;
                    goto name_start;
                default:
            }
            return Token.Name;
        case '0': .. case '9':
            cursor++;
            while (('0' <= *cursor && *cursor <= '9') || *cursor == '_')
                cursor++;
            return Token.Integer;
        default:
            return char_token(Token.Invalid);
    }
}

unittest { // Test empty lexer
    mixin(init_scan(""));
    assert(scan_type(start, cursor, end) == Token.Eof);
}

unittest { // Test empty lexer with whitespace
    mixin(init_scan("  \t\t\t\t    "));
    assert(scan_type(start, cursor, end) == Token.Eof);
}

unittest {
    mixin(init_scan("()[],.;->129400_81anb_wo283 if else loop break return let def"));

    bool test_scan(Token.Type t, size_t cursor_pos) {
        auto tok_start = cursor;
        return scan_type(tok_start, cursor, end) == t && cursor == start + cursor_pos;
    }

    assert(test_scan(Token.Lparen, 1));
    assert(test_scan(Token.Rparen, 2));
    assert(test_scan(Token.Lbracket, 3));
    assert(test_scan(Token.Rbracket, 4));
    assert(test_scan(Token.Comma, 5));
    assert(test_scan(Token.Dot, 6));
    assert(test_scan(Token.Semicolon, 7));
    assert(test_scan(Token.Rarrow, 9));
    assert(test_scan(Token.Integer, 18));
    assert(test_scan(Token.Name, 27));

    // Keywords are not preocessed at scan_type level
    assert(test_scan(Token.Name, 30)); // if
    assert(test_scan(Token.Name, 35)); // else
    assert(test_scan(Token.Name, 40)); // loop
    assert(test_scan(Token.Name, 46)); // break
    assert(test_scan(Token.Name, 53)); // return
    assert(test_scan(Token.Name, 57)); // let
    assert(test_scan(Token.Name, 61)); // def
}

version(unittest) {
    import arc.stringtable: StringTable;
    Lexer lex(const(char)[] src, StringTable *table) {
        return Lexer(src, table);
    }

    string init_scan(string test_string) {
        import std.format: format;

        return "
            const test_string = \"%s\";
            const(char)* start = test_string.ptr;
            const(char)* cursor = start;
            const char* end = cursor + %s;
        ".format(test_string, test_string.length);
    }
}