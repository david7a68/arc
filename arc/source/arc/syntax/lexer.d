module arc.syntax.lexer;

import arc.hash: Key, digest;
import arc.source: Span;
import arc.stringtable: StringTable;

struct Token {
    enum Type: ubyte {
        Invalid, Done, Eol = '\n',

        Lparen = '(', Rparen = ')', Lbracket = '[', Rbracket = ']', Lbrace = '{', Rbrace = '}',
        Comma = ',', Dot = '.', Semicolon = ';', Colon = ':',
        Ampersand = '&', Plus = '+', Minus = '-', Slash = '/', Star = '*', Caret = '^',
        Equals = '=', Less = '<', Greater = '>', Bang = '!',
        LessEqual = 128, GreaterEqual, EqualEqual, BangEqual,
        Rarrow, ColonColon, Label, DotDot,
        
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

struct Cursor {
    const char* start;
    const (char)* current;
    const char* end;

    this(const(char)[] text) {
        start = current = text.ptr;
        end = start + text.length;
    }

    const(char)[] text() {
        return start[0 .. end - start];
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

immutable end_of_section_tokens = [
    Token.Rbrace,
    Token.Eol,
    Token.Done,
    Token.Else
];

Token scan_token(ref Cursor cursor, Token.Type previous, Token.Type delim, StringTable strings) {
    const start = cursor;
    auto scan = scan_type(cursor, strings);

    if (scan.type.matches_one(end_of_section_tokens) && delim != Token.Invalid) {
        switch (previous) with (Token.Type) {
            case Rbrace:
            case Rparen:
            case Rbracket:
            case Name:
            case Integer:
            case Label:
            case Char:
            case Break:
            case Return:
            case Continue:
                cursor.current = start.current; // reset the cursor so we don't overwrite it
                return Token(delim, Span(cursor.index, 0));
            case Done:
                assert(scan.type == Token.Done && cursor.done);
                return Token(Token.Invalid);
            default:
                while (scan.type == Token.Eol)
                    scan = scan_type(cursor, strings);
        }
    }
    else if (scan.type == Token.Eol) {
        do {
            scan = scan_type(cursor, strings);
        } while (scan.type == Token.Eol);
    }
    else if (scan.type == previous && (previous == Token.Comma || previous == Token.Semicolon)) {
        do {
            scan = scan_type(cursor, strings);
        } while (scan.type == previous);
    }

    return scan;
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
Token scan_type(ref Cursor cursor, StringTable strings) {
    auto start = cursor;

    auto make_token(Token.Type t, int advance_n = 0, Key key = 0) {
        cursor.advance(advance_n);
        return Token(t, Span(start.index, cursor.index - start.index), key);
    }

    switch_start:
    if (cursor.done)
        return Token(Token.Done); 

    const c = *cursor.current;
    cursor.advance();
    switch (c) {
        case ' ':
        case '\t':
        case '\r':
            start.advance();
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
            return make_token(cast(Token.Type) c);
        case '-':
            if (!cursor.done && *cursor.current == '>')
                return make_token(Token.Rarrow, 1);
            return make_token(Token.Minus);
        case '.':
            if (!cursor.done && *cursor.current == '.')
                return make_token(Token.DotDot, 1);
            return make_token(Token.Dot);
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
            else return make_token(Token.Slash);
        case '=':
            if (!cursor.done && *cursor.current == '=')
                return make_token(Token.EqualEqual, 1);
            else // skip advancing here because we've already done it
                return make_token(Token.Equals);
        case '<':
            if (!cursor.done && *cursor.current == '=')
                return make_token(Token.LessEqual, 1);
            else
                return make_token(Token.Less);
        case '>':
            if (!cursor.done && *cursor.current == '=')
                return make_token(Token.GreaterEqual, 1);
            else
                return make_token(Token.Greater);
        case '!':
            if (!cursor.done && *cursor.current == '=')
                return make_token(Token.BangEqual, 1);
            else
                return make_token(Token.Bang);
        case '\'':
            if (cursor.done)
                return make_token(Token.Invalid);
            else if (*cursor.current == '\\') {
                cursor.advance(2);

                if (*cursor.current == '\'')
                    return make_token(Token.Char, 1);
                
                while (!cursor.done && *cursor.current != '\'')
                    cursor.advance();
                
                return make_token(Token.Invalid);
            }
            else {
                cursor.advance();
                if (*cursor.current == '\'')
                    return make_token(Token.Char, 1);
                else {
                    char_loop: if (!cursor.done) switch (*cursor.current) {
                        case 'a': .. case 'z':
                        case 'A': .. case 'Z':
                        case '0': .. case '9':
                        case '_':
                            cursor.advance();
                            goto char_loop;
                        default:
                    }

                    const key = digest(start.spanned_text(cursor));
                    return make_token(Token.Label, 0, key);
                }
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

            if (type == Token.Name)
                strings.insert(start.spanned_text(cursor), key);
            
            return make_token(type, 0, key);
        case '0': .. case '9':
            while (!cursor.done && (('0' <= *cursor.current && *cursor.current <= '9') || *cursor.current == '_'))
                cursor.advance();
            return make_token(Token.Integer, 0);
        default:
            return make_token(Token.Invalid, 1);
    }
}
