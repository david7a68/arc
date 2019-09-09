module arc.syntax.lexer;

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
        Invalid, Eof,
        Name, Integer,

        Lparen = '(', Rparen = ')',
        Lbracket = '[', Rbracket = ']',
        Comma = ',', Dot = '.',
        Plus = '+', Minus = '-', Slash = '/', Star = '*',
    }

    alias Type this;

    /// The type classification of the token
    Type type;
    /// A permanent pointer to the first character in this token
    const(char)* start;
    /// The length of the token in bytes
    size_t span;
}

/**
 * Input: Contiguous memory region containing arbitrary data.
 * Output: Sequence of classified sections of the arbitrary data.
 */
struct Lexer {
    /// A pointer to the current character in the source text
    const(char)* source_text;
    /// A pointer to the end of the source text
    const(char)* end_of_text;
    /// The ID with which the source text may be referred to
    /// The most recently analyzed token
    Token current;

    /**
     * Sets up the lexer to analyze a new source document
     */
    void reset(const(char)[] source) {
        source_text = source.ptr;
        end_of_text = source.ptr + source.length;
        current = Token();
    }

    /**
     * Lexically analyze the source text and stop after the first token in
     * found.
     *
     * This is the function that actually does all the work. It is fairly simple,
     * using a simple switch statement to determine the type of each character.
     */
    void lex() {
        auto start = source_text;

        switch_start:
        if (source_text >= end_of_text) {
            current = Token(Token.Eof, source_text, 0);
            return;
        }

        // A switch statement with label/goto was the simplest way to start.
        // @Optimize A character-table driven approach may be more performant.
        switch (*source_text) {
            case ' ':
            case '\n':
            case '\t':
            case '\r':
                source_text++;
                start++;
                goto switch_start;
            case '(':
                source_text++;
                current = Token(Token.Lparen, start, 1);
                break;
            case ')':
                source_text++;
                current = Token(Token.Rparen, start, 1);
                break;
            case '[':
                source_text++;
                current = Token(Token.Lbracket, start, 1);
                break;
            case ']':
                source_text++;
                current = Token(Token.Rbracket, start, 1);
                break;
            case ',':
                source_text++;
                current = Token(Token.Comma, start, 1);
                break;
            case '.':
                source_text++;
                current = Token(Token.Dot, start, 1);
                break;
            case '+':
                source_text++;
                current = Token(Token.Plus, start, 1);
                break;
            case '-':
                source_text++;
                current = Token(Token.Minus, start, 1);
                break;
            case '*':
                source_text++;
                current = Token(Token.Star, start, 1);
                break;
            case '/':
                source_text++;
                current = Token(Token.Slash, start, 1);
                break;
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
                current = Token(Token.Name, start, source_text - start);
                break;
            case '1': .. case '9':
                source_text++;
                while (('0' <= *source_text && *source_text <= '9') || *source_text == '_')
                    source_text++;
                
                current = Token(Token.Integer, start, source_text - start);
                break;
            default:
                source_text++;
                current = Token(Token.Invalid, start, source_text - start);
        }

        if (current.type == Token.Invalid) goto switch_start;
    }
}

// Test empty lexer
unittest {
    Lexer lexer;
    lexer.reset("");
    assert(lexer.current.type == Token.Invalid);
    lexer.lex();
    assert(lexer.current.type == Token.Eof);
}

// Test empty lexer with whitespace
unittest {
    Lexer lexer;
    lexer.reset("  \n\n\r\t\t\t\t    ");
    lexer.lex();
    assert(lexer.current.type == Token.Eof);
}

// Test isolated tokens
unittest {
    import std.range: zip;

    const strings = ["(", ")", "[", "]", ",", "129400_81", "anb_wo283"];
    const types = [Token.Lparen, Token.Rparen, Token.Lbracket, Token.Rbracket, Token.Comma, Token.Integer, Token.Name];
    assert(strings.length == types.length);

    Lexer lexer;
    foreach (pair; zip(strings, types)) {
        lexer.reset(pair[0]);
        lexer.lex();
        assert(lexer.current.type == pair[1]);
        assert(lexer.current.start[0 .. lexer.current.span] == pair[0]);
    }

    lexer.lex();
    assert(lexer.current.type == Token.Eof);
}

// Test isolated tokens surrounded by whitespace
unittest {
    import std.range: zip;
	import std.string: strip;

    const strings = ["  ( ", "  ) ", "   [", " ]   ", " , ", "   129400_81", " anb_wo283"];
    const types = [Token.Lparen, Token.Rparen, Token.Lbracket, Token.Rbracket, Token.Comma, Token.Integer, Token.Name];
    assert(strings.length == types.length);

    Lexer lexer;
    foreach (pair; zip(strings, types)) {
        lexer.reset(pair[0]);
        lexer.lex();

		const token = lexer.current;
        assert(token.type == pair[1]);

		const text = token.start[0 .. token.span];
        assert(text == pair[0].strip);
    }

    lexer.lex();
    assert(lexer.current.type == Token.Eof);
}

// Test adjescent tokens
unittest {
    const str = "[[]]],19281_2918 (_aiw19)_";
    const types = [
        Token.Lbracket, Token.Lbracket,Token.Rbracket, Token.Rbracket, Token.Rbracket, Token.Comma,
        Token.Integer, Token.Lparen, Token.Name, Token.Rparen, Token.Name
    ];

    Lexer lexer;
    lexer.reset(str);
    foreach (i; 0 .. types.length) {
        lexer.lex();

		const token = lexer.current;
        assert(token.type == types[i]);
    }

    lexer.lex();
    assert(lexer.current.type == Token.Eof);
}
