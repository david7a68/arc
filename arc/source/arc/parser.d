module arc.parser;

import arc.ast;
import arc.lexer;
import shard.array;
import shard.logger : Logger;
import shard.memory : Allocator;

void parse(const char[] source, Logger* logger, Allocator allocator, Allocator temp_allocator, out SyntaxTree ast) {
    ast.tokens = Array!Token(allocator);
    lex_tokens(source, ast.tokens);

    ast.nodes       = Array!AstNode(allocator);
    ast.node_data   = Array!NodeIndex(allocator);
    auto parser     = ParseContext(logger, &ast, temp_allocator);
    parser.parse();
}

struct ParseContext {
    // dfmt off
    Logger*     log;
    SyntaxTree* ast;
    Allocator   temp_allocator;
    TokenIndex  token_cursor;
    // dfmt on

    alias TT = Token.Type;

    bool done() { return token_cursor >= ast.tokens.length; }
    Token token() { return ast.tokens[token_cursor]; }

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
        const fail = ast.tokens[token_cursor].type != tt;
        if (fail)
            ast.errors.push_back(AstError.token_expect_mismatch(token_cursor, tt));
        return !fail;
    }

    void parse() {
        auto declarations = Array!NodeIndex(temp_allocator);
        while (!done()) {
            switch (token.type) with (TT) {
                case keyword_let:
                    const tok = take();
                    const ident = take(required(TT.identifier));
                    skip(required(TT.colon));

                    NodeIndex type, value;
                    if (token.type == TT.equals) {
                        type = 0;
                        value = expr();
                    }
                    else {
                        type = expr();
                        value = skip(token.type == TT.equals) ? expr() : 0;
                    }

                    ast.node_data.reserve(4);
                    const data_a = cast(NodeIndex) ast.node_data.push_back(0, 1); // hash ident
                    const data_b = cast(NodeIndex) ast.node_data.push_back(type, value);
                    auto let = AstNode.declaration!"let"(data_a, data_b);
                    declarations.push_back(cast(NodeIndex) ast.nodes.push_back(let));
                    skip(required(TT.semicolon));
                    break;

                case keyword_def:
                    const tok = take();
                    const ident = take(required(TT.identifier));
                    skip(required(TT.colon));

                    NodeIndex type, value;
                    if (token.type == TT.equals) {
                        type = 0;
                        value = expr();
                    }
                    else {
                        type = expr();
                        value = skip(token.type == TT.equals) ? expr() : 0;
                    }

                    ast.node_data.reserve(4);
                    const data_a = cast(NodeIndex) ast.node_data.push_back(0, 1); // hash ident
                    const data_b = cast(NodeIndex) ast.node_data.push_back(type, value);
                    auto def = AstNode.declaration!"def"(data_a, data_b);
                    declarations.push_back(cast(NodeIndex) ast.nodes.push_back(def));
                    skip(required(TT.semicolon));
                    break;

                default:
            }
        }
    }

    NodeIndex prefix() {
        // dfmt off
        switch (token.type) with (TT) {
            case identifier: {
                // intern ident string
                auto ident = AstNode.identifier(take(), token_cursor);
                return cast(NodeIndex) ast.nodes.push_back(ident);
            }
            case int_literal: {
                auto i_lit = AstNode.literal!"int"(take(), token_cursor);
                return cast(NodeIndex) ast.nodes.push_back(i_lit);
            }
            case char_literal: {
                auto c_lit = AstNode.literal!"char"(take(), token_cursor);
                return cast(NodeIndex) ast.nodes.push_back(c_lit);
            }
            case string_literal: {
                auto s_lit = AstNode.literal!"string"(take(), token_cursor);
                return cast(NodeIndex) ast.nodes.push_back(s_lit);
            }
            case keyword_not: {
                auto not = AstNode.unary!"not"(take(), expr());
                return cast(NodeIndex) ast.nodes.push_back(not);
            }
            case minus: {
                auto neg = AstNode.unary!"negate"(take(), expr());
                return cast(NodeIndex) ast.nodes.push_back(neg);
            }
            default:
                ast.errors.push_back(AstError.not_prefix_token(token_cursor));
                advance();
                return 0;
        }
        // dfmt on
    }

    NodeIndex expr() {
        return prefix();
    }
}
