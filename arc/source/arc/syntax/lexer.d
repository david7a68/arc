module arc.syntax.lexer;

import arc.data.hash: Key, digest;
import arc.data.source: Span;

struct Token {
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

    Type type;
    Span span;
    Key key;
}

bool matches_one(Token.Type type, const Token.Type[] types...) {
    foreach (t; types)
        if (type == t)
            return true;
    return false;
}

size_t read_tokens(const(char)[] text, Token[] buffer) {
    auto cursor = Cursor(text);

    // get first token
    buffer[0] = scan_token(cursor);

    // if the previous token was not EOF, and there is space in the buffer, scan token
    size_t i = 1;
    for (; buffer[i - 1].type != Token.Done && i < buffer.length; i++) 
        buffer[i] = scan_token(cursor);

    return cursor.current - text.ptr;
}

private:

struct Cursor {
    const (char)* start;
    const (char)* current;
    const (char)* end;

    this(const(char)[] text) {
        start = current = text.ptr;
        end = start + text.length;
    }

    uint index() in (current - start <= uint.max) {
        return cast(uint) (current - start);
    }

    bool done() {
        return current >= end;
    }

    void advance(uint n = 1) in (current + n <= end) {
        current += n;
    }

    const(char)[] spanned_text(ref Cursor other) {
        const lo = current < other.current ? current : other.current;
        const hi = current > other.current ? current : other.current;
        return lo[0 .. hi - lo];
    }
}


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
 * Identifies the first valid token in the text sequence from `*cursor.current` to 
 * `end`. If the first character in the text is not a valid start of a token,
 * a token of type `Invalid` will be returned.
 *
 * This function will identify keywords as distinct from symbols.
 */
Token scan_token(ref Cursor cursor) {
    auto start = cursor;

    auto make_token(Token.Type t, int advance_n = 0, Key key = 0) {
        cursor.advance(advance_n);
        return Token(t, Span(start.index, cursor.index - start.index), key);
    }

    auto make_op_token(Token.Type t, int advance_n = 0) {
        cursor.advance(advance_n);
        const key = digest(start.spanned_text(cursor));
        return Token(t, Span(start.index, cursor.index - start.index), key);
    }

    switch_start:
    if (cursor.done)
        return Token(Token.Done, Span(start.index, 0)); 

    const c = *cursor.current;
    cursor.advance();
    switch (c) {
        case ' ':
        case '\t':
        case '\r':
        case '\n':
            start.advance();
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
            if (!cursor.done && *cursor.current == '>')
                return make_token(Token.Rarrow, 1);
            return make_op_token(Token.Minus);
        case '.':
            return make_op_token(Token.Dot);
        case ':':
            if (!cursor.done && *cursor.current == ':')
                return make_token(Token.ColonColon, 1);
            else
                return make_token(Token.Colon);
        case '/':
            if (!cursor.done && *cursor.current == '/') {
                while (*cursor.current != '\n') cursor.advance();
                goto switch_start;
            }
            else return make_op_token(Token.Slash);
        case '=':
            if (!cursor.done && *cursor.current == '=')
                return make_op_token(Token.EqualEqual, 1);
            else // skip advancing here because we've already done it
                return make_op_token(Token.Equals);
        case '<':
            if (!cursor.done && *cursor.current == '=')
                return make_op_token(Token.LessEqual, 1);
            else
                return make_op_token(Token.Less);
        case '>':
            if (!cursor.done && *cursor.current == '=')
                return make_op_token(Token.GreaterEqual, 1);
            else
                return make_op_token(Token.Greater);
        case '!':
            if (!cursor.done && *cursor.current == '=')
                return make_op_token(Token.BangEqual, 1);
            else
                return make_op_token(Token.Bang);
        case '\'':
            if (cursor.done)
                return make_token(Token.Invalid);
            else if (*cursor.current == '\\') {
                auto key = digest(cursor.current[0 .. 2]);
                cursor.advance(2);

                if (*cursor.current == '\'')
                    return make_token(Token.Char, 1);
                
                while (!cursor.done && *cursor.current != '\'')
                    cursor.advance();
                
                return make_token(Token.Invalid);
            }
            else {
                auto key = digest(cursor.current[0 .. 1]);
                cursor.advance();
                if (*cursor.current == '\'')
                    return make_token(Token.Char, 1, key);
                else
                    return make_token(Token.Invalid);
            }
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
            loop_start: if (!cursor.done) switch (*cursor.current) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    cursor.advance();
                    goto loop_start;
                default:
            }

            const key = digest(start.spanned_text(cursor));
            const type = keywords.get(key, Token.Name);
            
            return make_token(type, 0, key);
        case '0': .. case '9':
            while (!cursor.done && (('0' <= *cursor.current && *cursor.current <= '9') || *cursor.current == '_'))
                cursor.advance();
            return make_token(Token.Integer, 0);
        default:
            return make_token(Token.Invalid, 1);
    }
}
