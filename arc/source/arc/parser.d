module arc.parser;

import arc.ast;
import arc.lexer;
import shard.array : Array;
import shard.logger : Logger;
import shard.memory : Allocator;

SyntaxTree parse(const Token[] tokens, Logger* logger, Allocator allocator, Allocator temp_allocator) {
    auto parser = ParseContext(tokens, logger, allocator, temp_allocator);
    parser.parse();

    SyntaxTree ast;
    parser.to_syntax_tree(ast);
    return ast;
}

struct ParseContext {
    // dfmt off
    Logger*         log;
    const Token[]   tokens;
    Allocator       temp_allocator;
    TokenIndex      token_cursor;

    // We can safely use the 'fat' arrays here because ParseContext is a
    // temporary object, which we don't expect to have many instances of
    // simultaneously.
    Array!AstNode   nodes;
    Array!NodeIndex node_data;
    Array!AstError  errors;
    // dfmt on

    this(const Token[] tokens, Logger* logger, Allocator allocator, Allocator temp_allocator) {
        // dfmt off
        this.log            = logger;
        this.tokens         = tokens;
        this.temp_allocator = temp_allocator;

        nodes               = Array!AstNode(allocator);
        node_data           = Array!NodeIndex(allocator);
        errors              = Array!AstError(allocator);
        // dfmt on
    }

    pragma(inline, true)
    void to_syntax_tree(out SyntaxTree ast) {
        nodes.unwrap(ast.nodes);
        node_data.unwrap(ast.node_data);
        errors.unwrap(ast.errors);
    }

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
            errors.push_back(AstError.token_expect_mismatch(token_cursor, tt));
        return !fail;
    }

    void parse() {
        auto root = nodes.push_back(AstNode(AstNode.Kind.root, 0, 0));

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

                    node_data.reserve_extra(3);
                    const data_a = cast(NodeIndex) node_data.push_back(ident.string_id.value); // hash ident
                    const data_b = cast(NodeIndex) node_data.push_back(type, value);
                    auto let = AstNode.declaration!"let"(data_a, data_b);
                    declarations.push_back(cast(NodeIndex) nodes.push_back(let));
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

                    node_data.reserve_extra(3);
                    const data_a = cast(NodeIndex) node_data.push_back(ident.string_id.value); // hash ident
                    const data_b = cast(NodeIndex) node_data.push_back(type, value);
                    auto def = AstNode.declaration!"def"(data_a, data_b);
                    declarations.push_back(cast(NodeIndex) nodes.push_back(def));
                    skip(required(TT.semicolon));
                    break;

                default:
            }
        }

        const first_decl = node_data.push_back(declarations[]);
        nodes[root].data_a = cast(uint) first_decl;
        nodes[root].data_b = cast(uint) declarations.length;
    }

    NodeIndex prefix() {
        // dfmt off
        switch (token.type) with (TT) {
            case identifier: {
                // intern ident string
                auto ident = AstNode.identifier(take(), token_cursor);
                return cast(NodeIndex) nodes.push_back(ident);
            }
            case int_literal: {
                auto i_lit = AstNode.literal!"int"(take(), token_cursor);
                return cast(NodeIndex) nodes.push_back(i_lit);
            }
            case char_literal: {
                auto c_lit = AstNode.literal!"char"(take(), token_cursor);
                return cast(NodeIndex) nodes.push_back(c_lit);
            }
            case string_literal: {
                auto s_lit = AstNode.literal!"string"(take(), token_cursor);
                return cast(NodeIndex) nodes.push_back(s_lit);
            }
            case keyword_not: {
                auto not = AstNode.unary!"not"(take(), expr());
                return cast(NodeIndex) nodes.push_back(not);
            }
            case minus: {
                auto neg = AstNode.unary!"negate"(take(), expr());
                return cast(NodeIndex) nodes.push_back(neg);
            }
            default:
                errors.push_back(AstError.not_prefix_token(token_cursor));
                advance();
                return 0;
        }
        // dfmt on
    }

    NodeIndex expr() {
        return prefix();
    }
}
