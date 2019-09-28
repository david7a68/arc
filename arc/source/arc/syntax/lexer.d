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
    import arc.hash: digest;

    keywords["if".digest] = Token.If;
    keywords["else".digest] = Token.Else;
    keywords["loop".digest] = Token.Loop;
    keywords["break".digest] = Token.Break;
    keywords["return".digest] = Token.Return;
    keywords["let".digest] = Token.Let;
    keywords["def".digest] = Token.Def;
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

        switch_start:
        if (source_text >= end_of_text) {
            return Token(Token.Eof, source_text, 0);
        }

        // A switch statement with label/goto was the simplest way to start.
        // @Optimize A character-table driven approach may be more performant.
        switch (*source_text) {
            case ' ':
            case '\t':
            case '\r':
                source_text++;
                start++;
                goto switch_start;
            case '\n':
                source_text++;
                return Token(Token.Eol, start, 1);
            case '(':
                source_text++;
                return Token(Token.Lparen, start, 1);
            case ')':
                source_text++;
                return Token(Token.Rparen, start, 1);
            case '[':
                source_text++;
                return Token(Token.Lbracket, start, 1);
            case ']':
                source_text++;
                return Token(Token.Rbracket, start, 1);
            case ',':
                source_text++;
                return Token(Token.Comma, start, 1);
            case '.':
                source_text++;
                return Token(Token.Dot, start, 1);
            case ';':
                source_text++;
                return Token(Token.Semicolon, start, 1);
            case ':':
                source_text++;
                return Token(Token.Colon, start, 1);
            case '+':
                source_text++;
                return Token(Token.Plus, start, 1);
            case '-':
                source_text++;
                if (*source_text == '>') {
                    source_text++;
                    return Token(Token.Rarrow, start, 2);
                }
                else {
                    return Token(Token.Minus, start, 1);
                }
            case '*':
                source_text++;
                return Token(Token.Star, start, 1);
            case '/':
                source_text++;
                return Token(Token.Slash, start, 1);
            case '^':
                source_text++;
                return Token(Token.Caret, start, 1);
            case '=':
                source_text++;
                return Token(Token.Equals, start, 1);
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
            case '_':
                source_text++;
                name_start:
                switch (*source_text) {
                    case 'a': .. case 'z':
                    case 'A': .. case 'Z':
                    case '0': .. case '9':
                    case '_':
                        source_text++;
                        goto name_start;
                    default:
                }

                auto id = table.insert(start[0 .. source_text - start]);
                return Token(keywords.get(id, Token.Name), start, source_text - start, id);
            case '0': .. case '9':
                source_text++;
                while (('0' <= *source_text && *source_text <= '9') || *source_text == '_')
                    source_text++;
                // We have to traverse the integer again in the parser
                // Yes, this is not exactly efficient. No, it doesn't really
                // matter right now
                return Token(Token.Integer, start, source_text - start);
            default:
                source_text++;
                current = Token(Token.Invalid, start, source_text - start);
        }

        // This part is to handle invalid tokens that fall through to switch::default
        if (next.type == Token.Invalid) goto switch_start;
        return current;
    }
}


version(unittest) {
    import arc.stringtable: StringTable;
    Lexer lex(const(char)[] src, StringTable *table) {
        return Lexer(src, table);
    }
}

unittest { // Test empty lexer
    auto lexer = lex("", null);
    assert(lexer.current.type == Token.Invalid);
    assert(lexer.scan_token.type == Token.Eof);
}

unittest { // Test empty lexer with whitespace
    assert(lex("  \r\t\t\t\t    ", null).scan_token.type == Token.Eof);
}

unittest { // Test isolated tokens
    import std.range: zip;
    import arc.stringtable: StringTable;

    const strings = ["(", ")", "[", "]", ",", ".", ";", "->", "129400_81", "anb_wo283"];
    const types = [Token.Lparen, Token.Rparen, Token.Lbracket, Token.Rbracket, Token.Comma, Token.Dot, Token.Semicolon, Token.Rarrow, Token.Integer, Token.Name];
    assert(strings.length == types.length);

    StringTable table;
    foreach (pair; zip(strings, types)) {
        auto t = lex(pair[0], &table).scan_token;
        assert(t.type == pair[1] && t.start[0 .. t.span] == pair[0]);
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

unittest { // Test adjescent tokens
    import arc.stringtable: StringTable;

    const str = "[[]]],19281_2918 (_aiw19)_if";
    const types = [
        Token.Lbracket, Token.Lbracket,Token.Rbracket, Token.Rbracket, Token.Rbracket, Token.Comma,
        Token.Integer, Token.Lparen, Token.Name, Token.Rparen, Token.Name
    ];

    StringTable table;
    auto lexer = lex(str, &table);
    foreach (i; 0 .. types.length) {
        assert(lexer.scan_token.type == types[i]);
    }

    assert(lexer.scan_token.type == Token.Eof);
}
