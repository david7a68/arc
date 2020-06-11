/**
 This module contains the functions and types used to parse a file into an
 abstract syntax tree.
 
 Error Model:
    The goals of defining an error model for the parser is to simplify and
    direct the way error information is recorded and distributed.

    Key observations:
        - Once a syntax error is detected within a declaration, the contents
          of the declaration cannot be trusted to be consistent, and no
          remedy is possible that would be simple enough that I would implement
          it.
        - First-pass type checking and symbol resolution can be accomplished
          to a limited degree throughout the rest of the compilation bundle so
          long as the name of a declaration is present. Type information would
          allow for even more analysis before things become senseless.
        - In fact, if the syntactical error is limited enough, parts of the
          bundle could actually be compiled separately in incremental builds.

    Consequences:
        - Top-level declarations should attempt to preserve their name and type
          in the presence of errors.
        - Any error not in file-scope should cause the parser to skip tokens
          until it can return to file-scope.
        - Information about nested statements adjescent to the error need not be
          preserved.
        - In order to impede as little as possible on the resource requirements
          for futher processing partially completed trees should be discarded,
          and their memory reclaimed.
        - Once parsing is completed, AstNode.Kind.Invalid should be found only as the
          immediate descendant of any declaration node.
 */
 module arc.syntax.parser;

import arc.data.ast;
import arc.data.ast_memory;
import arc.data.source: Span, merge_all;
import arc.reporter;
import arc.syntax.lexer: Token, TokenBuffer, matches_one;

enum Precedence {
    None,
    Assign,
    Logic,
    Equality,
    Compare,
    Sum,
    Product,
    Power,
    Call,
    Prefix
}

struct ParsingContext {
    TokenBuffer!4096 tokens;

    AstNodeAllocator nodes;
    Reporter* reporter;

    int indentation_level;

    alias nodes this;

    this(Reporter* reporter, AstNodeAllocator node_allocator) {
        this.reporter = reporter;
        this.nodes = node_allocator;
    }

    void begin(const char[] source_text) {
        tokens.begin(source_text);
        reporter.clear();

        indentation_level = tokens.current.type == Token.Lbrace ? 1 : 0;
    }

    bool is_done() { return tokens.current.type == Token.Done; }

    Token current() { return tokens.current; }

    void advance() {
        tokens.advance();

        if (tokens.current.type == Token.Lbrace)
            indentation_level++;
        else if (tokens.current.type == Token.Rbrace)
            indentation_level--;
    }

    Token take() { scope (exit) advance(); return tokens.current; }

    bool skip(Token.Type type) {
        const matched = type == tokens.current.type;
        if (matched) advance();
        return matched;
    }

    bool skip_required(Token.Type type) {
        const matched = match_required(type);
        if (matched) advance();
        return matched;
    }

    Token take_required(Token.Type type) {
        return match_required(type) ? take() : Token();
    }

    bool match_required(Token.Type type) {
        if (type == tokens.current.type) return true;

        const span = tokens.current.span;
        if (tokens.done)
            reporter.error(
                ArcError.UnexpectedEndOfFile, span,
                "The file ended unexpectedly. A %s was expected.", type
            );
        else
            reporter.error(
                ArcError.TokenExpectMismatch, span,
                "An unexpected token was encountered: Expected (%s), Encountered (%s)",
                type, tokens.source_text[span.start .. span.start + span.length]
            );
        return false;
    }

    Token resynchronize() {
        if (indentation_level == 0) return Token();

        while (tokens.current.type != Token.Done) {
            if (indentation_level == 0)
                break;
            advance();
        }

        auto close = take();
        if (tokens.current.type == Token.Semicolon)
            return take();
        return close;
    }
    
    AstNode* make_node(AstNode.Kind kind, Span prefix, AstNode* node) {
        if (is_valid(node)) return nodes.alloc(kind, prefix, node);
        scope (exit) nodes.free(node);
        return make_invalid(prefix.merge(node.span));
    }

    AstNode* make_node(AstNode.Kind kind, AstNode*[] seq, Span extra = Span()) {
        if (is_valid(seq)) return nodes.alloc(kind, seq, extra);

        scope (exit) nodes.free_seq(seq);
        auto span = extra;
        foreach (node; seq) span = span.merge(node.span);
        return make_invalid(span);
    }

    AstNode* make_node(AstNode.Kind kind, AstNode* first, AstNode* second) {
        if (is_valid(first, second)) return nodes.alloc(kind, first, second);
        scope (exit) nodes.free(first, second);
        return make_invalid(first.span.merge(second.span));
    }

    AstNode* make_invalid(Span span) { return nodes.alloc(AstNode.Kind.Invalid, span); }
}

auto parse_optional_type(ParsingContext* p) {
    return p.current.type != Token.Equals ? parse_type(p) : AstNode.inferred;
}

auto parse_optional_expr(ParsingContext* p) {
    return p.skip(Token.Equals) ? parse_expression(p, Precedence.Logic) : AstNode.inferred;
}

struct SequenceNodeInfo { AstNode.Kind kind; Token.Type open; Token.Type close; Token.Type delim = Token.None; }

AstNode* parse_seq(alias parse_member)(ParsingContext* p, SequenceNodeInfo info) {
    auto start = p.take_required(info.open).span;

    if (start.length == 0) return p.make_invalid(start);

    auto seq = SequenceBuilder(p.nodes);
    while (!p.current.type.matches_one(Token.Done, info.close)) {
        auto node = parse_member(p);

        if (!node.is_valid) {
            scope (exit) seq.abort();
            return p.make_invalid(start.merge(node.span));
        }

        if (info.delim) {
            if (!p.current.type.matches_one(info.close, Token.Done) && !p.skip_required(info.delim)) {
                scope (exit) seq.abort();
                return p.make_invalid(start.merge(node.span));
            }
            while (p.current.type == info.delim) p.advance();
        }

        seq.add(node);
    }

    auto span = p.current.span.merge(start);
    if (p.skip_required(info.close))
        return p.alloc(info.kind, span, seq.nodes);

    seq.abort();
    return p.make_invalid(span);
}

// ----------------------------------------------------------------------
//    _____  _          _                                 _        
//   / ____|| |        | |                               | |       
//  | (___  | |_  __ _ | |_  ___  _ __ ___    ___  _ __  | |_  ___ 
//   \___ \ | __|/ _` || __|/ _ \| '_ ` _ \  / _ \| '_ \ | __|/ __|
//   ____) || |_| (_| || |_|  __/| | | | | ||  __/| | | || |_ \__ \
//  |_____/  \__|\__,_| \__|\___||_| |_| |_| \___||_| |_| \__||___/
//
// ----------------------------------------------------------------------

AstNode* parse_assign(ParsingContext* p, AstNode* lhs) {
    p.skip_required(Token.Equals);
    auto rhs = parse_expression(p);
    return p.make_node(AstNode.Kind.Assign, lhs, rhs);
}

immutable block_sequence = SequenceNodeInfo(AstNode.Kind.Block, Token.Lbrace, Token.Rbrace);

AstNode* parse_if(ParsingContext* p) {
    auto start = p.take_required(Token.If).span;
    auto cond = parse_expression(p);
    auto body = parse_seq!parse_statement(p, block_sequence);
    auto base = p.skip(Token.Else) ? parse_statement(p) : AstNode.none;
    return p.make_node(AstNode.Kind.If, p.make_seq(cond, body, base), start);
}

AstNode* parse_loop(ParsingContext* p) {
    auto start = p.take_required(Token.Loop).span;
    auto body = parse_seq!parse_statement(p, block_sequence);
    return p.make_node(AstNode.Kind.Loop, start, body);
}

AstNode* parse_escape(ParsingContext* p, Token.Type token, AstNode.Kind kind) {
    return p.alloc(kind, p.take_required(token).span);
}

AstNode* parse_return(ParsingContext* p) {
    auto start = p.take_required(Token.Return).span;
    auto value = p.current.type != Token.Semicolon ? parse_expression(p) : AstNode.none;
    return p.make_node(AstNode.Kind.Return, start, value);
}

AstNode* parse_define(ParsingContext* p) {
    const start = p.take().span;
    auto name = parse_symbol(p, AstNode.Kind.Name);

    if (!p.skip_required(Token.Colon))
        return name.as_invalid(start.merge(name.span));

    auto type = parse_optional_type(p);
    auto expr = parse_optional_expr(p);
    return p.make_node(AstNode.Kind.Definition, p.make_seq(name, type, expr), start);
}

AstNode* parse_variable(ParsingContext* p, AstNode* name) {
    p.skip_required(Token.Colon);
    auto type = parse_optional_type(p);
    auto expr = parse_optional_expr(p);
    return p.make_node(AstNode.Kind.Variable, p.make_seq(name, type, expr));
}

AstNode* parse_statement(ParsingContext* p) {
    auto stmt = () {
        switch (p.current.type) with (Token.Type) {
            case Break:     return parse_escape(p, Token.Break, AstNode.Kind.Break);
            case Continue:  return parse_escape(p, Token.Continue, AstNode.Kind.Continue);
            case Def:       return parse_define(p);
            case If:        return parse_if(p);
            case Lbrace:    return parse_seq!parse_statement(p, block_sequence);
            case Loop:      return parse_loop(p);
            case Return:    return parse_return(p);
            default:
                auto prefix = parse_prefix(p);

                if (!prefix.is_valid) return prefix;

                if (prefix.kind == AstNode.Kind.Name && p.current.type == Token.Colon)
                    return parse_variable(p, prefix);

                auto infix = parse_infix(p, prefix);
                return p.current.type == Token.Equals ? parse_assign(p, infix) : infix;
        }
    } ();

    switch (stmt.kind) with (AstNode.Kind) {
        case Block: case If: case Loop: case Invalid:
            return stmt;

        default:
            auto semicolon = p.take_required(Token.Semicolon);
            if (semicolon.type == Token.Semicolon) {
                stmt.span = stmt.span.merge(semicolon.span);
                return stmt;
            }
            scope (exit) p.free(stmt);
            return p.make_invalid(stmt.span);
    }
}

// ----------------------------------------------------------------------
//   ______                                    _                    
//  |  ____|                                  (_)                   
//  | |__   __  __ _ __   _ __  ___  ___  ___  _   ___   _ __   ___ 
//  |  __|  \ \/ /| '_ \ | '__|/ _ \/ __|/ __|| | / _ \ | '_ \ / __|
//  | |____  >  < | |_) || |  |  __/\__ \\__ \| || (_) || | | |\__ \
//  |______|/_/\_\| .__/ |_|   \___||___/|___/|_| \___/ |_| |_||___/
//                | |                                               
//                |_|                                               
// ----------------------------------------------------------------------

AstNode* parse_symbol(ParsingContext* p, in AstNode.Kind kind) {
    auto t = p.take();
    return p.alloc(kind, t.span, t.key);
}

AstNode* parse_unary(ParsingContext* p, in AstNode.Kind kind) {
    return p.make_node(kind, p.take().span, parse_expression(p, Precedence.Call));
}

AstNode* parse_list(bool is_type)(ParsingContext* p, Token.Type open, Token.Type close) {
    static parse_member(bool is_type)(ParsingContext* p) {
        auto first = parse_expression(p, Precedence.Logic);
        if (p.current.type == Token.Colon) return parse_variable(p, first);

        auto parts = is_type
                   ? p.make_seq(AstNode.none, first, AstNode.inferred)
                   : p.make_seq(AstNode.none, AstNode.inferred, first);
        return p.make_node(AstNode.Kind.Variable, parts);
    }

    auto list = parse_seq!(parse_member!is_type)(p, SequenceNodeInfo(AstNode.Kind.List, open, close, Token.Comma));

    if (p.current.type != Token.Rarrow) return list;
    else if (is_type)                   return parse_function_type(p, list);
    else                                return parse_function(p, list);
}

AstNode* parse_binary(alias parse_fn)(ParsingContext* p, AstNode* lhs, in Infix op) {
    if (op.skip_token) p.advance();
    auto rhs = parse_fn(p, cast(Precedence) (op.prec + op.is_left_associative));
    return p.make_node(op.kind, lhs, rhs);
}

AstNode* parse_function(ParsingContext* p, AstNode* list) {
    p.advance();
    // () -> expr | () -> {} | () -> expr {}
    auto first = p.current.type == Token.Lbrace ? AstNode.inferred : parse_expression(p);
    auto parts = p.current.type == Token.Lbrace
               ? p.make_seq(list, first, parse_seq!parse_statement(p, block_sequence))
               : p.make_seq(list, AstNode.inferred, first);
    return p.make_node(AstNode.Kind.Function, parts);
}

AstNode* parse_expression(ParsingContext* p, in Precedence prec = Precedence.Assign) {
    return parse_infix(p, parse_prefix(p), prec);
}

AstNode* parse_prefix(ParsingContext* p) {
    const token = p.current;
    switch (token.type) with (Token.Type) {
        case Bang: case Not:    return parse_unary(p, AstNode.Kind.Not);
        case Minus:             return parse_unary(p, AstNode.Kind.Negate);
        case Import:            return parse_unary(p, AstNode.Kind.Import);
        case Name:              return parse_symbol(p, AstNode.Kind.Name);
        case Integer:           return parse_symbol(p, AstNode.Kind.Integer);
        case Char:              return parse_symbol(p, AstNode.Kind.Char);
        case String:            return parse_symbol(p, AstNode.Kind.String);
        case Lparen:            return parse_list!false(p, Lparen, Rparen);
        case Lbracket:          return parse_list!false(p, Lbracket, Rbracket);
        default:
    }

    if (token.type == Token.Done)
        p.reporter.error(ArcError.UnexpectedEndOfFile, token.span, "Unexpected end of file while parsing expression.");
    else with (Token.Type) {
        auto s = token.type.matches_one(Semicolon, Comma, Rparen, Rbracket, Rbrace)
               ? tprint("Expression ended unexpectedly with a %s.", token.type)
               : tprint("The token \"%s\" cannot start a prefix expression.", token.type);
        p.reporter.error(ArcError.TokenExpectMismatch, token.span, s);
    }

    return p.make_invalid(token.span);
}

AstNode* parse_infix(ParsingContext* p, AstNode* expr, in Precedence prec = Precedence.Assign) {
    static immutable Infix[256] infixes = [
        Token.Less          : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.Less),
        Token.LessEqual     : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.LessEqual),
        Token.Greater       : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.Greater),
        Token.GreaterEqual  : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.GreaterEqual),
        Token.EqualEqual    : Infix(Precedence.Equality,    true,   true,   AstNode.Kind.Equal),
        Token.BangEqual     : Infix(Precedence.Equality,    true,   true,   AstNode.Kind.NotEqual),
        Token.And           : Infix(Precedence.Logic,       true,   true,   AstNode.Kind.And),
        Token.Or            : Infix(Precedence.Logic,       true,   true,   AstNode.Kind.Or),
        Token.Plus          : Infix(Precedence.Sum,         true,   true,   AstNode.Kind.Add),
        Token.Minus         : Infix(Precedence.Sum,         true,   true,   AstNode.Kind.Subtract),
        Token.Star          : Infix(Precedence.Product,     true,   true,   AstNode.Kind.Multiply),
        Token.Slash         : Infix(Precedence.Product,     true,   true,   AstNode.Kind.Divide),
        Token.Caret         : Infix(Precedence.Power,       false,  true,   AstNode.Kind.Power),
        Token.Dot           : Infix(Precedence.Call,        true,   true,   AstNode.Kind.Access),
        Token.ColonColon    : Infix(Precedence.Call,        true,   true,   AstNode.Kind.StaticAccess),
        Token.Name          : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
        Token.Integer       : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
        Token.Char          : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
        Token.String        : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
        Token.Lparen        : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
        Token.Lbracket      : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call)
    ];

    for (Infix i = infixes[p.current.type]; expr.is_valid && prec <= i.prec; i = infixes[p.current.type])
        expr = parse_binary!parse_expression(p, expr, i);

    return expr;
}

struct Infix { Precedence prec; bool is_left_associative, skip_token; AstNode.Kind kind; }

// ----------------------------------------------------------------------
//   _______                       
//  |__   __|                      
//     | | _   _  _ __    ___  ___ 
//     | || | | || '_ \  / _ \/ __|
//     | || |_| || |_) ||  __/\__ \
//     |_| \__, || .__/  \___||___/
//          __/ || |               
//         |___/ |_|               
// ----------------------------------------------------------------------

AstNode* parse_function_type(ParsingContext* p, AstNode* list) {
    p.skip_required(Token.Rarrow);
    return p.make_node(AstNode.Kind.FunctionType, list, parse_type(p));
}

AstNode* parse_type(ParsingContext* p, Precedence prec = Precedence.Assign) {
    return parse_type_infix(p, parse_type_prefix(p), prec);
}

AstNode* parse_type_prefix(ParsingContext* p) {
    switch (p.current.type) with (Token.Type) {
        case Star:      return p.make_node(AstNode.Kind.PointerType, p.take().span, parse_type(p));
        case Name:      return parse_symbol(p, AstNode.Kind.Name);
        case Lparen:    return parse_list!true(p, Lparen, Rparen);
        case Lbracket:  return parse_list!true(p, Lbracket, Rbracket);
        default:
    }

    p.reporter.error(
        ArcError.TokenExpectMismatch, p.current.span,
        "A type expression may be an access, symbol, list, function type, or call, not a %s", p.current.type
    );

    return p.make_invalid(p.take().span);
}

AstNode* parse_type_infix(ParsingContext* p, AstNode* expr, Precedence prec) {
    static immutable Infix[256] ops = [
        Token.Dot       : Infix(Precedence.Call, true, true,    AstNode.Kind.Access),
        Token.Lparen    : Infix(Precedence.Call, true, false,   AstNode.Kind.Call),
        Token.Lbracket  : Infix(Precedence.Call, true, false,   AstNode.Kind.Call)
    ];

    for (Infix i = ops[p.current.type]; expr.is_valid && prec <= i.prec; i = ops[p.current.type])
        expr = parse_binary!parse_type(p, expr, i);

    return expr;
}
