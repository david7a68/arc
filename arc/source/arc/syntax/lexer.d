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

        And, Or,
        If, Else, Loop, Break, Return, Continue,
        Def,
    }

    alias Type this;

    ///
    Type type;
    ///
    Span span;
    ///
    Key key;
}

/// Returns `true` if `type` matches one of the types in `types`.
bool matches_one(Token.Type type, const Token.Type[] types...) {
    foreach (t; types)
        if (type == t)
            return true;
    return false;
}

/**
 Eagerly consumes the text to produce up to `buffer.length` tokens, inserts the
 tokens into `buffer`, and returns the number of characters read. All tokens'
 spans are relative to the beginning of the text provided. If you want to adjust
 the spans' locations, simply add an offset to `span.start`.

 Upon reaching the end of the file, this function will insert a 0-length Done
 token to indicate the end of the document.
 */
size_t read_tokens(const(char)[] text, Token[] buffer) {
    auto base = cast(size_t) text.ptr;
    auto current = text.ptr;
    auto end = current + text.length;

    // get first token
    buffer[0] = scan_token(base, current, end);

    // if the previous token was not EOF, and there is space in the buffer, scan token
    size_t i = 1;
    for (; buffer[i - 1].type != Token.Done && i < buffer.length; i++) 
        buffer[i] = scan_token(base, current, end);

    return current - text.ptr;
}

private:

/// Hashmap of reserved keywords and their corresponding token types
immutable Token.Type[Key] keywords;

shared static this() {
    keywords[digest("and")] = Token.And;
    keywords[digest("or")] = Token.Or;
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
Token scan_token(size_t base, ref const(char)* current, ref const(char*) end) {
    auto start = current;

    auto final_span() { return Span((cast(size_t) start) - base, current - start); }

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
    if (current >= end)
        return Token(Token.Done, Span((cast(size_t) start) - base, 0)); 
    
    const c = *current;
    current++;

    switch (c) {
        case ' ':
        case '\t':
        case '\r':
        case '\n':
            start++;
            goto switch_start;

        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case ',':
        case ';':
            return make_token(cast(Token.Type) c);

        case '+':
        case '*':
        case '^':
        case '&':
            return make_op_token(cast(Token.Type) c);

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
            if (current >= end) {
                return make_token(Token.Invalid);
            }
            else {
                const content_length = *current == '\\' ? 2 : 1;
                const key = digest(current[0 .. content_length]);
                current += content_length;

                if (*current == '\'')
                    return make_token(Token.Char, 1, key);
                else
                    return make_token(Token.Invalid);
            }

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
            return make_token(Token.Integer, 0);

        default:
            return make_token(Token.Invalid, 1);
    }
}
