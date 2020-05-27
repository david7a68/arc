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
        - Once parsing is completed, AstNode.Invalid should be found only as the
          immediate descendant of any declaration node.
 */
 module arc.syntax.parser2;

import arc.data.ast2;
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

    AstNodeAllocator* nodes;
    Reporter* reporter;

    int indentation_level;

    alias nodes this;

    this(Reporter* reporter, AstNodeAllocator* node_allocator) {
        this.reporter = reporter;
        this.nodes = node_allocator;
    }

    void begin(const char[] source_text) { tokens.begin(source_text); }

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
        if (match_required(type)) return take();
        return Token();
    }

    bool match_required(Token.Type type) {
        if (type == tokens.current.type) return true;
        
        const span = tokens.current.span;
        if (tokens.done) {
            if (!reporter.has_error(ArcError.UnexpectedEndOfFile))
                reporter.error(
                    ArcError.UnexpectedEndOfFile, span,
                    "The file ended unexpectedly. A %s was expected.",
                    type);
        }
        else
            reporter.error(
                ArcError.TokenExpectMismatch, span,
                "An unexpected token was encountered: Expected (%s), Encountered (%s)",
                type, tokens.source_text[span.start .. span.start + span.length]);
        return false;
    }

    void resynchronize() { while (indentation_level > 0 && !tokens.done) advance(); }
}

auto parse_optional_type(ParsingContext* p) {
    // return p.current.type != Token.Equals ? parse_type_expr(p) : AstNode.inferred;
    return p.current.type != Token.Equals ? parse_expression(p) : AstNode.inferred;
}

auto parse_optional_expr(ParsingContext* p) {
    return p.skip(Token.Equals) ? parse_expression(p, Precedence.Logic) : AstNode.inferred;
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

AstNode* parse_block(ParsingContext* p) {
    auto start = p.take().span;

    auto seq = p.alloc_sequence_buffer();
    while (!p.current.type.matches_one(Token.Done, Token.Rbrace)) {
        seq.add(parse_statement(p));

        if (!seq[$ - 1].is_valid) {
            p.abort(seq);
            return p.alloc(AstNode.Invalid, start);
        }
    }

    auto end = p.current.span;
    if (p.skip_required(Token.Rbrace))
        return p.alloc(AstNode.Block, start.merge(end), p.alloc_sequence(seq));
    
    p.abort(seq);
    return p.alloc(AstNode.Invalid, start.merge(end));
}

AstNode* parse_variable(bool is_statement)(ParsingContext* p, AstNode* name) {
    p.skip_required(Token.Colon);

    auto type = parse_optional_type(p);
    auto expr = parse_optional_expr(p);

    auto span = merge_all(name.span, type.span, expr.span);
    static if (is_statement) {
        const semicolon = p.take_required(Token.Semicolon);

        if (semicolon.type == Token.Semicolon)
            return p.alloc(AstNode.Variable, span.merge(semicolon.span), p.alloc_sequence(name, type, expr));

        p.free(type, expr);
        return name.as_invalid(span);
    }
    else {
        if (type.is_valid && expr.is_valid) 
            return p.alloc(AstNode.Variable, span, p.alloc_sequence(name, type, expr));
        
        p.free(type, expr);
        return name.as_invalid(span);
    }
}

AstNode* parse_statement(ParsingContext* p) {
    switch (p.current.type) with (Token.Type) {
        case Lbrace: return parse_block(p);
        default:
            auto prefix = prefixes[p.current.type](p);

            if (prefix.kind == AstNode.Kind.Name && p.current.type == Token.Colon)
                return parse_variable!true(p, prefix);
            
            auto expr = prefix.is_valid ? parse_infix(p, prefix) : prefix;

            if (expr.is_valid && p.skip_required(Token.Semicolon)) return expr;

            scope (exit) p.free(expr.children);
            return expr.as_invalid(expr.span);
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

AstNode* parse_symbol(AstNode.Kind kind)(ParsingContext* p) {
    auto t = p.take();
    return p.alloc(kind, t.span, t.key);
}

AstNode* parse_unary(AstNode.Kind kind)(ParsingContext* p) {
    auto t = p.take();
    auto operand = parse_expression(p, Precedence.Call);

    if (operand.is_valid)
        return p.alloc(kind, t.span, operand);

    return operand.as_invalid(t.span);
}

AstNode* parse_list(Token.Type closing)(ParsingContext* p) {
    static parse_member(ParsingContext* p) {
        auto first = parse_expression(p, Precedence.Logic);

        if (!first.is_valid) {
            p.free(first.children);
            return first.as_invalid(first.span);
        }

        auto span = first.span;
        if (p.skip(Token.Colon)) {
            auto type = parse_optional_type(p);
            auto expr = parse_optional_expr(p);
            span = merge_all(span, type.span, expr.span);

            if (type.is_valid && expr.is_valid)
                return p.alloc(AstNode.Variable, span, p.alloc_sequence(first, type, expr));

            p.free(type, expr);
            p.free(first.children);
            return first.as_invalid(first.span);
        }

        return p.alloc(AstNode.Variable, span, p.alloc_sequence(AstNode.none, AstNode.inferred, first));
    }

    const begin = p.take().span;
    while (p.current.type == Token.Comma) p.advance();

    auto seq = p.alloc_sequence_buffer();
    if (!p.current.type.matches_one(closing, Token.Done)) while (true) {
        seq.add(parse_member(p));

        if (!seq[$ - 1].is_valid) {
            scope (exit) p.abort(seq);
            return p.alloc(AstNode.Invalid, begin.merge(seq[$ - 1].span));
        }
        else if (p.current.type.matches_one(closing, Token.Done)) {
            break;
        }
        else {
            if (!p.skip_required(Token.Comma)) {
                scope (exit) p.abort(seq);
                return p.alloc(AstNode.Invalid, begin.merge(seq[$ - 1].span));
            }

            while (p.current.type == Token.Comma) p.advance();
            if (p.current.type.matches_one(closing, Token.Done)) break;
        }
    }

    const end = p.current.span;

    if (!p.skip_required(closing)) {
        p.abort(seq);
        return p.alloc(AstNode.Invalid, begin.merge(end));
    }

    auto members = p.alloc_sequence(seq);
    auto list = p.alloc(AstNode.List, begin.merge(end), members);

    if (p.current.type == Token.Rarrow)
        return parse_function(p, list);
    return list;
}

AstNode* parse_binary(ParsingContext* p, AstNode* lhs, Infix op) {
    if (op.skip_token) p.advance();

    auto rhs = parse_expression(p, cast(Precedence) (op.prec + op.is_left_associative));
    if (rhs.is_valid)
        return p.alloc(op.kind, p.alloc_sequence(lhs, rhs));

    auto node = p.alloc(AstNode.Invalid, lhs.span.merge(rhs.span));
    p.free(lhs, rhs);
    return node;
}

AstNode* parse_function(ParsingContext* p, AstNode* list) {
    p.advance();

    AstNode* expr;
    if (p.current.type != Token.Lbrace) {
        expr = parse_expression(p, Precedence.Prefix);

        if (!expr.is_valid) {
            scope (exit) p.free(list);
            return expr.as_invalid(list.span.merge(expr.span));
        }
    }
    else expr = AstNode.inferred;

    if (p.current.type == Token.Lbrace) {
        auto body = parse_block(p);
        if (body.is_valid)
            return p.alloc(AstNode.Function, p.alloc_sequence(list, expr, body));

        scope (exit) p.free(list, expr, body);
        return p.alloc(AstNode.Invalid, list.span.merge(body.span));
    }

    if (expr.kind != AstNode.Inferred)
        return p.alloc(AstNode.Function, p.alloc_sequence(list, AstNode.inferred, expr));

    // The function parse_expression() should have caught any token errors...
    assert(false, "ICE Unreachable: All other cases should be handled by parse_expression().");
}

AstNode* parse_expression(ParsingContext* p, Precedence prec = Precedence.Assign) {
    auto expr = prefixes[p.current.type](p);
    return parse_infix(p, expr, prec);
}

AstNode* parse_infix(ParsingContext* p, AstNode* expr, Precedence prec = Precedence.Assign) {
    for (Infix i = infixes[p.current.type]; expr.is_valid && prec <= i.prec; i = infixes[p.current.type])
        expr = parse_binary(p, expr, i);

    if (expr.is_valid) return expr;

    p.free(expr.children);
    return expr.as_invalid(expr.span);
}

alias PrefixFn = AstNode* function(ParsingContext*);

immutable prefixes = () {
    PrefixFn[256] parsers   = (p) {
        if (p.current.type == Token.Semicolon) {
            p.reporter.error(
                ArcError.Code.TokenExpectMismatch, p.current.span,
                "Unexpected end of expression. An unexpected semicolon was encountered.");
            return p.alloc(AstNode.Kind.Invalid, p.current.span);
        }

        p.reporter.error(
            ArcError.TokenNotAnExpression, p.current.span,
            "The token \"%s\" cannot start a prefix expression.",
            p.current.type);
        return p.alloc(AstNode.Kind.Invalid, p.take().span);
    };

    parsers[Token.Done]     = (p) {
        p.reporter.error(
            ArcError.UnexpectedEndOfFile, p.current.span,
            "Unexpected end of file while parsing source.");
        return p.alloc(AstNode.Kind.Invalid, p.current.span);
    };

    with (Token.Type) {
        parsers[Minus]      = &parse_unary!(AstNode.Negate);
        parsers[Bang]       = &parse_unary!(AstNode.Not);
        parsers[Not]        = &parse_unary!(AstNode.Not);

        parsers[Name]       = &parse_symbol!(AstNode.Name);
        parsers[Integer]    = &parse_symbol!(AstNode.Integer);
        parsers[Char]       = &parse_symbol!(AstNode.Char);

        parsers[Lparen]     = &parse_list!Rparen;
        parsers[Lbracket]   = &parse_list!Rbracket;
    }

    return parsers;
} ();

struct Infix { Precedence prec; bool is_left_associative, skip_token; AstNode.Kind kind; }

immutable infixes = () {
    Infix[256] ops;

    with (Token.Type) with (Precedence) {
        ops[Less]           = Infix(Compare,    true,   true,   AstNode.Less);
        ops[LessEqual]      = Infix(Compare,    true,   true,   AstNode.LessEqual);
        ops[Greater]        = Infix(Compare,    true,   true,   AstNode.Greater);
        ops[GreaterEqual]   = Infix(Compare,    true,   true,   AstNode.GreaterEqual);
        ops[EqualEqual]     = Infix(Equality,   true,   true,   AstNode.Equal);
        ops[BangEqual]      = Infix(Equality,   true,   true,   AstNode.NotEqual);
        ops[And]            = Infix(Logic,      true,   true,   AstNode.And);
        ops[Or]             = Infix(Logic,      true,   true,   AstNode.Or);
        ops[Plus]           = Infix(Sum,        true,   true,   AstNode.Add);
        ops[Minus]          = Infix(Sum,        true,   true,   AstNode.Subtract);
        ops[Star]           = Infix(Product,    true,   true,   AstNode.Multiply);
        ops[Slash]          = Infix(Product,    true,   true,   AstNode.Divide);
        ops[Caret]          = Infix(Power,      false,  true,   AstNode.Power);
        ops[Dot]            = Infix(Call,       true,   true,   AstNode.Access);


        ops[Name]           = Infix(Call,       false,  false,  AstNode.Call);
        ops[Integer]        = Infix(Call,       true,   false,  AstNode.Call);
        ops[Char]           = Infix(Call,       true,   false,  AstNode.Call);
        ops[Lparen]         = Infix(Call,       true,   false,  AstNode.Call);
        ops[Lbracket]       = Infix(Call,       true,   false,  AstNode.Call);
    }

    return ops;
} ();
