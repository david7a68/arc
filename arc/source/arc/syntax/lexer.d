module arc.syntax.lexer;

import arc.hash: Key, digest;
import arc.syntax.location: Span, SpannedText;

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

struct Lexer {
    import std.container.array: Array;

    const(char)* cursor;
    const(char)* end;
    /// The span of text that this lexer is working on.
    SpannedText span;

    Token current;

    Array!(Token.Type) eol_stack;

    alias front = current;
    alias popFront = advance;

    this(SpannedText span) {
        cursor = span.text.ptr;
        end = cursor + span.text.length;
        this.span = span;
        advance();
    }

    bool empty() {
        return current.type == Token.Done;
    }

    void advance() {
        const eol_type = eol_stack.length > 0 ? eol_stack.back : Token.Invalid;
        auto scan = scan_token(cursor, end, current.type, eol_type);
        current = Token(scan.type, span.get_span(scan.text).span, scan.key);
    }

    void push_eol_delimiter(Token.Type t) {
        eol_stack.insertBack(t);
    }

    void pop_eol_delimiter() {
        eol_stack.removeBack();
    }
}

struct ScanResult {
    Token.Type type;
    const(char)[] text;
    Key key;
}

immutable end_of_section_tokens = [
    Token.Rbrace,
    Token.Eol,
    Token.Done,
    Token.Else
];

ScanResult scan_token(ref const(char)* cursor, const char* end, Token.Type previous, Token.Type eol_type) {
    const start = cursor;
    auto scan = scan_type(cursor, end);

    if (scan.type.matches_one(end_of_section_tokens) && eol_type != Token.Invalid) {
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
                cursor = start; // reset the cursor so we don't overwrite it
                return ScanResult(eol_type, scan.text[0..0]);
            case Done:
                assert(scan.type == Token.Done);
                return ScanResult(Token.Invalid, scan.text[0..0]);
            default:
                while (scan.type == Token.Eol)
                    scan = scan_type(cursor, end);
        }
    }
    else if (scan.type == Token.Eol) {
        do {
            scan = scan_type(cursor, end);
        } while (scan.type == Token.Eol);
    }
    else if (scan.type == previous && (previous == Token.Comma || previous == Token.Semicolon)) {
        do {
            scan = scan_type(cursor, end);
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
 * Identifies the first valid token in the text sequence from `*cursor` to 
 * `end`. If the first character in the text is not a valid start of a token,
 * a token of type `Invalid` will be returned.
 *
 * This function will identify keywords as distinct from symbols.
 */
ScanResult scan_type(ref const(char)* cursor, const char* end) {
    auto start = cursor;

    auto make_token(Token.Type t, int advance_n = 0, Key key = 0) {
        cursor += advance_n;
        assert(cursor <= end);
        return ScanResult(t, start[0 .. cursor - start], key);
    }

    switch_start:
    if (cursor >= end)
        return ScanResult(Token.Done, (cursor)[0 .. 0]); 

    const c = *cursor;
    cursor++;
    switch (c) {
        case ' ':
        case '\t':
        case '\r':
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
            return make_token(cast(Token.Type) c);
        case '-':
            if (cursor != end && *cursor == '>')
                return make_token(Token.Rarrow, 1);
            return make_token(Token.Minus);
        case '.':
            if (cursor <= end && *cursor == '.')
                return make_token(Token.DotDot, 1);
            return make_token(Token.Dot);
        case ':':
            if (cursor <= end && *cursor == ':')
                return make_token(Token.ColonColon, 1);
            else
                return make_token(Token.Colon);
        case '/':
            if (cursor <= end && *cursor == '/') {
                while (*cursor != '\n') cursor++;
                goto switch_start;
            }
            else return make_token(Token.Slash);
        case '=':
            if (cursor != end && *cursor == '=')
                return make_token(Token.EqualEqual, 1);
            else // skip advancing here because we've already done it
                return make_token(Token.Equals);
        case '<':
            if (cursor != end && *cursor == '=')
                return make_token(Token.LessEqual, 1);
            else
                return make_token(Token.Less);
        case '>':
            if (cursor != end && *cursor == '=')
                return make_token(Token.GreaterEqual, 1);
            else
                return make_token(Token.Greater);
        case '!':
            if (cursor != end && *cursor == '=')
                return make_token(Token.BangEqual, 1);
            else
                return make_token(Token.Bang);
        case '\'':
            if (cursor == end)
                return make_token(Token.Invalid);
            else if (*cursor == '\\') {
                cursor += 2;

                if (*cursor == '\'')
                    return make_token(Token.Char, 1);
                
                while (cursor != end && *cursor != '\'')
                    cursor++;
                
                return make_token(Token.Invalid);
            }
            else {
                cursor++;
                if (*cursor == '\'')
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

                    const key = digest(start[0 .. cursor - start]);
                    return make_token(Token.Label, 0, key);
                }
            }
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
            loop_start: if (cursor < end) switch (*cursor) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    cursor++;
                    goto loop_start;
                default:
            }

            const key = digest(start[0 .. cursor - start]);
            return make_token(keywords.get(key, Token.Name), 0, key);
        case '0': .. case '9':
            while (cursor < end && (('0' <= *cursor && *cursor <= '9') || *cursor == '_'))
                cursor++;
            return make_token(Token.Integer, 0);
        default:
            return make_token(Token.Invalid, 1);
    }
}
