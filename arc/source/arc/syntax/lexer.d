module arc.syntax.lexer;

import arc.data.hash : digest, Key;
import arc.data.source : Span;
import arc.util : case_of;

/// A `Token` is the smallest discrete unit of the source text that the compiler
/// operates on.
struct Token {
    /// Represents the distinct classes of tokens that the are used in the compiler.
    enum Type : ubyte {
        // dfmt off
        None, Invalid, Done,

        Lparen = '(', Rparen = ')', Lbracket = '[', Rbracket = ']', Lbrace = '{', Rbrace = '}',
        Comma = ',', Dot = '.', Semicolon = ';', Colon = ':',
        Ampersand = '&', Plus = '+', Minus = '-', Slash = '/', Star = '*', Caret = '^',
        Equals = '=', Less = '<', Greater = '>', Bang = '!',
        LessEqual = 128, GreaterEqual, EqualEqual, BangEqual,
        Rarrow, ColonColon,
        
        Name, Integer, Char, String,

        And, Or, Not,
        If, Else, Loop, Break, Return, Continue,
        Def, Import,
        // dfmt on
    }

    alias Type this;

    ///
    Type type;
    ///
    Span span;
    union {
        ///
        Key key;
        ///
        ulong value;
    }
}

/// Returns `true` if `type` matches one of the types in `types`.
bool matches_one(Token.Type type, const Token.Type[] types...) {
    foreach (t; types)
        if (type == t)
            return true;
    return false;
}

/**
 A TokenBuffer offers a buffered 1-token window over a source text.
 */
struct TokenBuffer(size_t buffer_size) {
private:
    uint _current_token_index;
    uint _buffer_span_offset;
    uint _next_buffer_index;

public:
    Token[buffer_size] tokens;
    const(char)[] source_text;
    Token current;
    bool done;

    this(const(char)[] text, uint span_offset = 0) {
        source_text = text;
        _buffer_span_offset = span_offset;
        fill_buffer();
        current = tokens[0];
        done = current.type == Token.Done;
    }

    void begin(const(char)[] text, uint span_offset = 0) {
        this = typeof(this)(text, span_offset);
    }

    void advance() {
        _current_token_index++;
        if (_current_token_index == tokens.length)
            fill_buffer();

        current = tokens[_current_token_index];
        done = current.type == Token.Done;
    }

    void fill_buffer() {
        const base = source_text.ptr;
        auto current = source_text.ptr + _next_buffer_index; // we allow indexing past the buffer because scan_token handles it for us.
        const end = source_text.length + base;

        debug tokens[] = Token.init;

        // get first token, might be Done
        tokens[0] = scan_token(base, current, end, _buffer_span_offset);
        for (size_t i = 1; tokens[i - 1].type != Token.Done && i < tokens.length;
                i++)
            tokens[i] = scan_token(base, current, end, _buffer_span_offset);

        const read = current - (source_text.ptr + _next_buffer_index);
        _next_buffer_index += read;
        _current_token_index = 0;
    }
}

private:

/// Hashmap of reserved keywords and their corresponding token types
immutable Token.Type[Key] keywords;

shared static this() {
    keywords[digest("and")] = Token.And;
    keywords[digest("or")] = Token.Or;
    keywords[digest("not")] = Token.Not;
    keywords[digest("if")] = Token.If;
    keywords[digest("else")] = Token.Else;
    keywords[digest("loop")] = Token.Loop;
    keywords[digest("break")] = Token.Break;
    keywords[digest("return")] = Token.Return;
    keywords[digest("continue")] = Token.Continue;
    keywords[digest("def")] = Token.Def;
    keywords[digest("import")] = Token.Import;
}

/**
 Identifies the first valid token in the text sequence from `*current` to
 `end`. If the first character in the text is not a valid start of a token, a
 token of type `Invalid` will be returned. Note that this function does no
 currently process multiple `Invalid` characters as a single token.

 This function will identify keywords as distinct from symbols.
 */
Token scan_token(const char* base, ref const(char)* current, ref const(char*) end, uint span_offset) {
    auto start = current;

    auto final_span() {
        return Span(cast(uint)(start - base) + span_offset, cast(uint)(current - start));
    }

    auto make_token(Token.Type t, size_t advance_n, Key key = 0) {
        current += advance_n;
        return Token(t, final_span(), key);
    }

    auto make_2_op(char second, Token.Type two_char_type, Token.Type one_char_type) {
        static const length = [2, 1];
        Token.Type[2] ttype = [two_char_type, one_char_type];
        bool is_single_char = (current + 1 == end) | (*(current + 1) != second); //bit to avoid short circuit
        return make_token(ttype[is_single_char], length[is_single_char]);
    }

    while (start < end) {
        current = start;

        switch (*current) with (Token.Type) {
        case '#':
            while (start < end && *start != '\n')
                start++;
            start++; // we can safely cross past end, handled in loop conditional
            continue;

        // dfmt off
        mixin(case_of(" \t\r\n"));
            start++;
            continue;

        mixin(case_of("()[]{}.,;+*^&/"));
            return make_token(cast(Token.Type)*current, 1);

        case ':': return make_2_op(':', ColonColon,     Colon);
        case '-': return make_2_op('>', Rarrow,         Minus);
        case '=': return make_2_op('=', EqualEqual,     Equals);
        case '<': return make_2_op('=', LessEqual,      Less);
        case '>': return make_2_op('=', GreaterEqual,   Greater);
        case '!': return make_2_op('=', BangEqual,      Bang);
        // dfmt on

        case '\'':
            current++;
            const length = *current == '\\' ? 2 : 1;
            const key = digest(current[0 .. length]);

            if (current < end && *(current + length) == '\'')
                return make_token(Char, length + 1, key);
            return make_token(Invalid, current - start);

        case '"':
            current++;
            for (; current < end; current++) {
                const c = *current;
                if (c == '\\')
                    current++;
                if (c == '"')
                    break;
            }

            const length = current - start;
            if (current == end)
                make_token(Invalid, length);
            return make_token(String, 1, digest(start[0 .. length]));

        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
        loop: for (; current < end; current++) switch (*current) {
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
            case '0': .. case '9':
            case '_':
                continue;
            default:
                break loop;
            }

            const key = digest(start[0 .. current - start]);
            const type = keywords.get(key, Name);
            return make_token(type, 0, key);

        case '0': .. case '9':
            while (current < end && (('0' <= *current
                    && *current <= '9') || *current == '_'))
                current++;
            return make_token(Integer, 0, string_to_int(start[0 .. current - start]));

        default:
            return make_token(Invalid, 1);
        }
    }

    return Token(Token.Done, Span(cast(uint)(end - base), 0));
}

ulong string_to_int(const char[] text) {
    ulong value = text[0] - '0';

    foreach (c; text[1 .. $]) {
        if (c == '_')
            continue;
        value = value * 10 + (c - '0');
    }

    return value;
}
