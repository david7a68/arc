module arc.lexer;

import arc.string_store : StringStore, StringId;
import shard.array : Array;

struct Token {
    enum Type : ubyte {
        // dfmt off
        none, invalid, done,

        l_paren = '(', r_paren = ')', l_bracket = '[', r_bracket = ']', l_brace = '{', r_brace = '}',
        comma = ',', dot = '.', semicolon = ';', colon = ':',
        ampersand = '&', plus = '+', minus = '-', slash = '/', star = '*', caret = '^',
        equals = '=', less = '<', greater = '>', bang = '!',
        less_equal = 128, greater_equal, equal_equal, bang_equal,
        r_arrow, r_fat_arrow, colon_colon,
        
        identifier, int_literal, char_literal, string_literal,

        keyword_and, keyword_or, keyword_not,
        keyword_if, keyword_else, keyword_loop,
        keyword_break, keyword_return, keyword_continue,
        keyword_def, keyword_let,

        keyword_import,
        // dfmt on
    }

    Type type;
    uint start;
    uint end;
    StringId string_id;

    bool opCast(T: bool)() const { return type != Type.none; }
}

/// Returns `true` if `type` matches one of the types in `types`.
bool matches_one(Token.Type type, const Token.Type[] types...) {
    foreach (t; types)
        if (type == t)
            return true;
    return false;
}

void lex_tokens(const char[] source, StringStore* strings, ref Array!Token tokens) {
    const start = source.ptr;
    const end = source.ptr + source.length;
    auto cursor = source.ptr;

    // Approximate source-chars-to-tokens ratio, reserving at least 1 token.
    tokens.reserve((source.length + 8) / 8);

    while (true) {
        const t = _scan_token(start, cursor, end, strings);
        tokens.push_back(t);
        if (t.type == Token.Type.done)
            break;
    }
}

private:

/// Hashmap of reserved keywords and their corresponding token types
immutable Token.Type[const char[]] _keywords;

shared static this() {
    _keywords["and"] = Token.Type.keyword_and;
    _keywords["or"] = Token.Type.keyword_or;
    _keywords["not"] = Token.Type.keyword_not;
    _keywords["if"] = Token.Type.keyword_if;
    _keywords["else"] = Token.Type.keyword_else;
    _keywords["loop"] = Token.Type.keyword_loop;
    _keywords["break"] = Token.Type.keyword_break;
    _keywords["return"] = Token.Type.keyword_return;
    _keywords["continue"] = Token.Type.keyword_continue;
    _keywords["def"] = Token.Type.keyword_def;
    _keywords["let"] = Token.Type.keyword_let;
    _keywords["import"] = Token.Type.keyword_import;
}

/**
 Identifies the first valid token in the text sequence from `*current` to
 `end`. If the first character in the text is not a valid start of a token, a
 token of type `invalid` will be returned. Note that this function does no
 currently process multiple `invalid` characters as a single token.

 This function will identify keywords as distinct from symbols.
 */
Token _scan_token(const char* base, ref const(char)* current, const char* end, StringStore* strings) {
    auto start = current;

    auto make_token(Token.Type t, size_t advance_n) {
        current += advance_n;
        return Token(t, cast(uint) (start - base), cast(uint) (current - start));
    }

    auto make_2_op(char second, Token.Type two_char_type, Token.Type one_char_type) {
        static const length = [2, 1];
        Token.Type[2] ttype = [two_char_type, one_char_type];
        const is_single_char = (current + 1 == end) | (*(current + 1) != second); //bit to avoid short circuit
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
        mixin(_case_of(" \t\r\n"));
            start++;
            continue;

        mixin(_case_of("()[]{}.,;+*^&/"));
            return make_token(cast(Token.Type)*current, 1);

        case ':': return make_2_op(':', colon_colon,    colon);
        case '-': return make_2_op('>', r_arrow,        minus);
        case '<': return make_2_op('=', less_equal,     less);
        case '>': return make_2_op('=', greater_equal,  greater);
        case '!': return make_2_op('=', bang_equal,     bang);
        // dfmt on

        case '=':
            current++;
            if (*current == '=')
                return make_token(equal_equal, 1);
            else if (*current == '>')
                return make_token(r_fat_arrow, 1);
            else
                return make_token(equals, 0);

        case '\'': {
            current++;
            const length = *current == '\\' ? 2 : 1;

            if (current < end && *(current + length) == '\'')
                return make_token(char_literal, length + 1);
            return make_token(invalid, current - start);
        }
        case '"': {
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
                return make_token(invalid, length);
            // TODO: unescape string, strings.save(str, unescaped_str);
            return make_token(string_literal, 1);
        }
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
        case '_': {
            loop: for (; current < end; current++) switch (*current) {
                case 'a': .. case 'z':
                case 'A': .. case 'Z':
                case '0': .. case '9':
                case '_':
                    continue;
                default:
                    break loop;
                }

                auto str = start[0 .. current - start];
                const type = _keywords.get(str, identifier);
                auto token = make_token(type, 0);
                token.string_id = type == identifier ? strings.save(str) : StringId();
                return token;
        }
        case '0': .. case '9': {
            while (current < end && (('0' <= *current
                    && *current <= '9') || *current == '_'))
                current++;
            auto token = make_token(int_literal, 0);
             token.string_id = strings.save(start[0 .. current - start]);
            return token;
        }                                                           
        default:
            return make_token(invalid, 1);
        }
    }

    return Token(Token.Type.done, cast(uint)(end - base));
}

ulong _string_to_int(const char[] text) {
    ulong value = text[0] - '0';

    foreach (c; text[1 .. $]) {
        if (c == '_')
            continue;
        value = value * 10 + (c - '0');
    }

    return value;
}

string _case_of(T)(const T[] c...) {
    import std.algorithm : map;
    import std.array : join;
    import std.conv: to;

    return c.map!(v => "case " ~ v.to!int.to!string ~ ": ").join().to!string;
}
