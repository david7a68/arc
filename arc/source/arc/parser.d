module arc.parser;

import arc.ast;
import arc.lexer;
import shard.array;
import shard.logger : Logger;
import shard.memory : Allocator;

void parse(const char[] source, Logger* logger, Allocator allocator, out SyntaxTree ast) {
    ast.tokens = Array!Token(allocator);
    lex_tokens(source, tokens);

    ast.nodes  = Array!AstNode(allocator);
    ast.data   = Array!NodeIndex(allocator);
    ast.parser = ParseContext(logger, ast, allocator);
    parser.parse();
}

struct ParseContext {
    // dfmt off
    Logger*     log;
    SyntaxTree* ast;
    size_t      token_cursor;
    // dfmt on

    alias TT = Token.Type;

    bool done() { return token_cursor >= tokens.length; }
    Token token() { return tokens[token_cursor]; }

    void advance() { token_cursor++; }

    Token take(bool should_advance = true) {
        assert(!done());
        auto t = token();
        if (should_advance) token_cursor++;
        return t;
    }

    bool skip(bool should_skip) {
        assert(!done());
        token_cursor += should_skip;
        return should_skip;
    }

    bool required(TT tt) {
        assert(!done());
        const fail = tokens[token_cursor].type != tt;
        if (fail)
            ast.errors.push_back(AstError.token_expect_mismatch(token_cursor, tt));
        return !fail;
    }

    void parse() {
        while (!done()) {
            // do stuff
        }
    }
}
