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

    NodeIndex prefix() {
        // dfmt off
        switch (token.type) with (TT) {
            case identifier: {
                // intern ident string
                auto ident = AstNode.identifier(take(), token_cursor);
                return ast.nodes.push_back(ident);
            }
            case int_literal: {
                auto i_lit = AstNode.literal!"int"(take(), token_cursor);
                return ast.nodes.push_back(i_lit);
            }
            case char_literal: {
                auto c_lit = AstNode.literal!"char"(take(), token_cursor);
                return ast.nodes.push_back(c_lit);
            }
            case string_literal: {
                auto s_lit = AstNode.literal!"string"(take(), token_cursor);
                return ast.nodes.push_back(s_lit);
            }
            case not: {
                auto not = AstNode.unary!"not"(take(), expr());
                ast.nodes.push_back(not);
                return not;
            }
            case minus: {
                auto neg = AstNode.unary!"negate"(take(), expr());
                ast.nodes.push_back(neg);
                return neg;
            }
        }
        // dfmt on
    }

    NodeIndex expr() {
        return prefix();
    }
}
