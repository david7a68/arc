module arc.syntax.lexer;

import arc.data.hash: Key, digest;
import arc.data.source: Span;

/// A `Token` is the smallest discrete unit of the source text that the compiler
/// operates on.
struct Token {
    /// Represents the distinct classes of tokens that the are used in the compiler.
    enum Type: ubyte {
        Invalid, Done,

        Lparen = '(', Rparen = ')', Lbracket = '[', Rbracket = ']', Lbrace = '{', Rbrace = '}',
        Comma = ',', Dot = '.', Semicolon = ';', Colon = ':',
        Ampersand = '&', Plus = '+', Minus = '-', Slash = '/', Star = '*', Caret = '^',
        Equals = '=', Less = '<', Greater = '>', Bang = '!',
        LessEqual = 128, GreaterEqual, EqualEqual, BangEqual,
        Rarrow, ColonColon,
        
        Name, Integer, Char,

        And, Or, Not,
        If, Else, Loop, Break, Return, Continue,
        Def,
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
    size_t _current_token_index;
    size_t _buffer_span_offset;
    size_t _next_buffer_index;

public:
    Token[buffer_size] tokens;
    const(char)[] source_text;
    Token current;
    bool done;

    this(const(char)[] text, size_t span_offset = 0) {
        source_text = text;
        _buffer_span_offset = span_offset;
        fill_buffer();
        current = tokens[_current_token_index];
        done = current.type == Token.Done;
    }

    void begin(const(char)[] text, size_t span_offset = 0) {
        this = typeof(this)(text, span_offset);
    }

    void advance() {
        if (_current_token_index + 1 < tokens.length)
            _current_token_index++;
        else {
            fill_buffer();
            _current_token_index = 0;
        }
        current = tokens[_current_token_index];
        done = current.type == Token.Done;
    }

    void fill_buffer() {
        auto base = source_text.ptr;
        auto current = source_text.ptr + _next_buffer_index; // we allow indexing past the buffer because scan_token handles it for us.
        auto end = source_text.length + base;

        debug tokens[] = Token.init;

        // get first token
        tokens[0] = scan_token(base, current, end);
        tokens[0].span.start += _buffer_span_offset;

        // if the previous token was not EOF, and there is space in the buffer, scan token
        size_t i = 1;
        for (; tokens[i - 1].type != Token.Done && i < tokens.length; i++) {
            tokens[i] = scan_token(base, current, end);
            tokens[i].span.start += _buffer_span_offset;
        }

        const read = current - (source_text.ptr + _next_buffer_index);
        _next_buffer_index += read;
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
}

/**
 Identifies the first valid token in the text sequence from `*current` to
 `end`. If the first character in the text is not a valid start of a token, a
 token of type `Invalid` will be returned. Note that this function does no
 currently process multiple `Invalid` characters as a single token.

 This function will identify keywords as distinct from symbols.
 */
Token scan_token(const char* base, ref const(char)* current, ref const(char*) end) {
    auto start = current;

    string case_of(const char[] c) {
        import std.algorithm: map;
        import std.array: join;
        import std.conv: to;
        return c.map!(v => "case "d ~ v.to!int.to!dstring ~ ": "d).join().to!string;
    }

    auto final_span() { return Span(cast(uint) (start - base), cast(uint) (current - start)); }

    auto make_token(Token.Type t, int advance_n = 0, Key key = 0) {
        current += advance_n;
        return Token(t, final_span(), key);
    }

    auto make_op_token(Token.Type t, int advance_n = 0) {
        current += advance_n;
        const key = digest(start[0 .. current - start]);
        return Token(t, final_span(), key);
    }

    switch_start:
    current = start;

    if (current >= end)
        return Token(Token.Done, Span(cast(uint) (start - base), 0)); 

    const c = *current;
    current++;

    switch (c) {
        mixin(case_of(" \t\r\n"));
            start++;
            goto switch_start;

        mixin(case_of("()[]{},;"));
            return make_token(cast(Token.Type) c);

        mixin(case_of("+*^&"));
            return make_op_token(cast(Token.Type) c);

        case '#':
            while (start < end && *start != '\n') start++;
            start += start != end; // to skip the \n, true is 1, false is 0
            goto switch_start;

        case '-':
            if (current < end && *current == '>')
                return make_token(Token.Rarrow, 1);
            return make_op_token(Token.Minus);

        case '.':
            return make_op_token(Token.Dot);

        case ':':
            if (current < end && *current == ':')
                return make_token(Token.ColonColon, 1);
            else
                return make_token(Token.Colon);

        case '/':
            if (current < end && *current == '/') {
                while (*current != '\n') current++;
                goto switch_start;
            }
            else return make_op_token(Token.Slash);

        case '=':
            // Branchless version, need to performance test
            // static immutable eq_type    = [Token.Equals,    Token.EqualEqual];
            // static immutable eq_adv     = [0,               1];
            // const second_eq = current < end && *current == '=';
            // return make_op_token(eq_type[second_eq], eq_adv[second_eq]);
            if (current < end && *current == '=')
                return make_op_token(Token.EqualEqual, 1);
            else
                return make_op_token(Token.Equals);

        case '<':
            if (current < end && *current == '=')
                return make_op_token(Token.LessEqual, 1);
            else
                return make_op_token(Token.Less);

        case '>':
            if (current < end && *current == '=')
                return make_op_token(Token.GreaterEqual, 1);
            else
                return make_op_token(Token.Greater);

        case '!':
            if (current < end && *current == '=')
                return make_op_token(Token.BangEqual, 1);
            else
                return make_op_token(Token.Bang);

        case '\'':
            if (current >= end)
                return make_token(Token.Invalid);
            
            const content_length = *current == '\\' ? 2 : 1;
            const key = digest(current[0 .. content_length]);
            current += content_length;

            if (*current == '\'')
                return make_token(Token.Char, 1, key);
            return make_token(Token.Invalid);

        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
            loop_start: if (current < end) switch (*current) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    current++;
                    goto loop_start;
                default:
            }

            const key = digest(start[0 .. current - start]);
            const type = keywords.get(key, Token.Name);
            return make_token(type, 0, key);

        case '0': .. case '9':
            while (current < end && (('0' <= *current && *current <= '9') || *current == '_'))
                current++;
            return make_token(Token.Integer, 0, string_to_int(start[0 .. current - start]));

        default:
            return make_token(Token.Invalid, 1);
    }
}

ulong string_to_int(const char[] text) {
    ulong value = text[0] - '0';

    foreach (c; text[1 .. $]) {
        if (c == '_') continue;
        
        value *= 10;
        value += c - '0';
    }

    return value;
}
