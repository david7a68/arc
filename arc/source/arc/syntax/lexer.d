module arc.syntax.lexer;

import std.typecons: Tuple;
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
    // const(char)* start;
    /// The length of the token in bytes
    // size_t span;
    
    SpannedText span;
    
    /// The unique key that identifies a string. This member is unused if the
    /// token is not an name
    Key key;
}


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
    
    import arc.syntax.location: SpannedText;

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

    SpannedText source;

    this(SpannedText source, StringTable* table) {
        source_text = source.text.ptr;
        end_of_text = source.text.ptr + source.length;
        eol_type_stack.clear();
        this.table = table;
        this.source = source;
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
                current = Token(eol_type_stack.back);
                break;
            default:
                do {
                    current = next;
                    next = scan_type(source_text, end_of_text).refine(table).locate(source);
                } while (current.type == Token.Eol);
            }
        }
        else {
            current = next;
            next = scan_type(source_text, end_of_text).refine(table).locate(source);
        }
    }
}

alias RefinedToken = Tuple!(Token.Type, "type", const(char)[], "text", Key, "key");


/**
* Converts the result given from `refine` into a token.
*
* Once spans with absolute positioning are implemented, this will find the
* absolute position of the token.
*/
Token locate(RefinedToken result, SpannedText source) {
    import arc.syntax.location: Span;

    return Token(result.type, source.get_span(result.text), result.key);
}

/**
 * Refines the result given by `scan_type`, further separating tokens of the 
 * same character class such as keywords and names.
 */
RefinedToken refine(ScanResult result, StringTable* table) {
    import std.typecons: tuple;
    Key key;

    if (result.type == Token.Name) {
        key = table.insert(result.text);
        result.type = keywords.get(key, Token.Name);
    }
    return tuple!("type", "text", "key")(result.type, result.text, key);
}

unittest { // Test keyword detection
    import arc.stringtable: StringTable;
    import std.range: zip;

    const keys = "if else loop break return let def";
    const types = [
        Token.If, Token.Else, Token.Loop, Token.Break, Token.Return, Token.Let, Token.Def
    ];

    mixin(init_scan(keys));
    StringTable table;
    foreach (type; types)
        assert(scan_type(cursor, end).refine(&table).type == type);
}

alias ScanResult = Tuple!(Token.Type, "type", const(char)[], "text");

/**
 * Scans a character buffer from `cursor` to `end`, producing a tuple of the 
 * type of the first token, and its length. The scanner does not distinguish
 * between tokens of the same character class, such as keywords and identifiers,
 * leaving that up to a higher-level scanner.
 * 
 * Returns: (type: Token.Type, text: const(char)[])
 */
auto scan_type(ref const(char)* cursor, const char* end) {
    import std.typecons: Tuple, tuple;

    auto start = cursor;

    Tuple!(Token.Type, "type", const(char)[], "text") make_token(Token.Type t, int advance_n = 1) {
        cursor += advance_n;
        return tuple!("type", "text")(t, start[0 .. cursor - start]);
    }

    switch_start:
    if (cursor >= end)
        return make_token(Token.Eof, 0); 

    // @Optimize A character-table driven approach may be more performant.
    switch (*cursor) {
        case ' ':
        case '\t':
        case '\r':
            cursor++;
            start++;
            goto switch_start;
        case '\n':
            return make_token(Token.Eol);
        case '(':
            return make_token(Token.Lparen);
        case ')':
            return make_token(Token.Rparen);
        case '[':
            return make_token(Token.Lbracket);
        case ']':
            return make_token(Token.Rbracket);
        case ',':
            return make_token(Token.Comma);
        case '.':
            return make_token(Token.Dot);
        case ';':
            return make_token(Token.Semicolon);
        case ':':
            return make_token(Token.Colon);
        case '+':
            return make_token(Token.Plus);
        case '-':
            cursor++;
            if (*cursor == '>') // advance only by one
                return make_token(Token.Rarrow, 1);
            else // skip advancing here because we've already done it
                return make_token(Token.Minus, 0);
        case '*':
            return make_token(Token.Star);
        case '/':
            return make_token(Token.Slash);
        case '^':
            return make_token(Token.Caret);
        case '=':
            return make_token(Token.Equals);
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
            return make_token(Token.Name, 0);
        case '0': .. case '9':
            cursor++;
            while (('0' <= *cursor && *cursor <= '9') || *cursor == '_')
                cursor++;
            return make_token(Token.Integer, 0);
        default:
            return make_token(Token.Invalid);
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
    mixin(init_scan("()[],.;->129400_81anb_wo283 if else loop break return let def"));

    bool test_scan(Token.Type t, const char[] text, size_t cursor_pos) {
        auto tok = scan_type(cursor, end);
        return tok.type == t && tok.text == text && cursor == start + cursor_pos;
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

    // Keywords are not preocessed at scan_type level
    assert(test_scan(Token.Name,        "if", 30));
    assert(test_scan(Token.Name,        "else", 35));
    assert(test_scan(Token.Name,        "loop", 40));
    assert(test_scan(Token.Name,        "break", 46));
    assert(test_scan(Token.Name,        "return", 53));
    assert(test_scan(Token.Name,        "let", 57));
    assert(test_scan(Token.Name,        "def", 61));
}

version(unittest) {
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