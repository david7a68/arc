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

    this(SpannedText span) {
        cursor = span.text.ptr;
        end = cursor + span.text.length;
        this.span = span;
    }

    bool empty() {
        return current.type == Token.Done;
    }

    void advance() {
        const eol_type = eol_stack.length > 0 ? eol_stack.back : Token.Invalid;
        auto scan = scan_token(&cursor, end, current.type, eol_type);
        
        Key key;
        if (scan.type == Token.Label)
            key = digest(scan.text);
        else if (scan.type == Token.Name) {
            key = digest(scan.text);
            scan.type = keywords.get(key, Token.Name);
        }

        current = Token(scan.type, span.get_span(scan.text).span, key);
    }

    void push_eol_delimiter(Token.Type t) {
        eol_stack.insertBack(t);
    }

    void pop_eol_delimiter() {
        eol_stack.removeBack();
    }

    alias drop = advance;

    Token take() {
        scope(exit) advance();
        return current;
    }

    bool skip(Token.Type t) {
        if (current.type != t)
            return false;
        
        advance();
        return true;
    }

    bool matches_one(Token.Type[] types...) {
        foreach (type; types)
            if (current.type == type)
                return true;
        return false;
    }
}

Lexer scan_tokens(SpannedText span) {
    auto l = Lexer(span);
    l.advance();
    return l;
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

struct ScanResult {
    Token.Type type;
    const(char)[] text;
}

ScanResult scan_token(const(char)** cursor, const char* end, Token.Type previous, Token.Type eol_type) {
    static immutable end_of_section_tokens = [Token.Rbrace, Token.Eol, Token.Done, Token.Break, Token.Return, Token.Continue, Token.If, Token.Else, Token.Loop];

    const start = *cursor;
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
            // We can test for these keywords because the previous token has
            // already been processed for all keywords.
            case Break:
            case Return:
            case Continue:
                *cursor = start;
                return ScanResult(eol_type, scan.text[0..0]);
            case Done:
                assert(scan.type == Token.Done);
                return ScanResult(Token.Invalid, scan.text[0..0]);
            default:
                do {
                    scan = scan_type(cursor, end);
                } while (scan.type == Token.Eol);
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

ScanResult scan_type(const(char)** cursor, const char* end) {
    auto start = *cursor;

    auto make_token(Token.Type t, int advance_n) {
        *cursor += advance_n;
        assert(*cursor <= end);
        return ScanResult(t, start[0 .. *cursor - start]);
    }

    switch_start:
    if (*cursor >= end)
        return ScanResult(Token.Done, (*cursor)[0 .. 0]); 

    // @Optimize A character-table driven approach may be more performant.
    switch (**cursor) {
        case ' ':
        case '\t':
        case '\r':
            (*cursor)++;
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
            return make_token(cast(Token.Type) **cursor, 1);
        case '-':
            (*cursor)++;
            if (*cursor != end && **cursor == '>')
                return make_token(Token.Rarrow, 1);
            return make_token(Token.Minus, 0);
        case '.':
            (*cursor)++;
            if (*cursor <= end && **cursor == '.')
                return make_token(Token.DotDot, 1);
            return make_token(Token.Dot, 0);
        case ':':
            (*cursor)++;
            if (*cursor <= end && **cursor == ':')
                return make_token(Token.ColonColon, 1);
            else
                return make_token(Token.Colon, 0);
        case '/':
            (*cursor)++;
            if (*cursor <= end && **cursor == '/') {
                while (**cursor != '\n') (*cursor)++;
                goto switch_start;
            }
            else return make_token(Token.Slash, 0);
        case '=':
            (*cursor)++;
            if (*cursor != end && **cursor == '=')
                return make_token(Token.EqualEqual, 1);
            else // skip advancing here because we've already done it
                return make_token(Token.Equals, 0);
        case '<':
            (*cursor)++;
            if (*cursor != end && **cursor == '=')
                return make_token(Token.LessEqual, 1);
            else
                return make_token(Token.Less, 0);
        case '>':
            (*cursor)++;
            if (*cursor != end && **cursor == '=')
                return make_token(Token.GreaterEqual, 1);
            else
                return make_token(Token.Greater, 0);
        case '!':
            (*cursor)++;
            if (*cursor != end && **cursor == '=')
                return make_token(Token.BangEqual, 1);
            else
                return make_token(Token.Bang, 0);
        case '\'':
            (*cursor)++;
            if (*cursor == end)
                return make_token(Token.Invalid, 0);
            else if (**cursor == '\\') {
                *cursor += 2;

                if (**cursor == '\'')
                    return make_token(Token.Char, 1);
                
                while (*cursor != end && **cursor != '\'')
                    cursor++;
                
                return make_token(Token.Invalid, 0);
            }
            else {
                (*cursor)++;

                if (**cursor == '\'')
                    return make_token(Token.Char, 1);
                else {
                    char_loop: if (*cursor < end) switch (**cursor) {
                        case 'a': .. case 'z':
                        case 'A': .. case 'Z':
                        case '0': .. case '9':
                        case '_':
                            (*cursor)++;
                            goto char_loop;
                        default:
                    }

                    return make_token(Token.Label, 0);
                }
            }
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_':
            (*cursor)++;
            loop_start: if (*cursor < end) switch (**cursor) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    (*cursor)++;
                    goto loop_start;
                default:
            }
            return make_token(Token.Name, 0);
        case '0': .. case '9':
            (*cursor)++;
            while (*cursor < end && (('0' <= **cursor && **cursor <= '9') || **cursor == '_'))
                (*cursor)++;
            return make_token(Token.Integer, 0);
        default:
            return make_token(Token.Invalid, 1);
    }
}
