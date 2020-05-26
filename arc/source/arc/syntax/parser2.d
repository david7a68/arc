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
    AstNodeAllocator* nodes;
    TokenBuffer!4096 tokens;
    Reporter* reporter;

    int indentation_level;

    this(Reporter* reporter, AstNodeAllocator* node_allocator) {
        this.reporter = reporter;
        this.nodes = node_allocator;
    }

    void begin(const char[] source_text) { tokens.begin(source_text); }

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
        const matched = match_required(type);
        if (matched) return take();
        return Token();
    }

    bool match_required(Token.Type type) {
        if (type == tokens.current.type) return true;
        
        const span = tokens.current.span;
        if (tokens.done)
            reporter.error(
                ArcError.UnexpectedEndOfFile, span,
                "The file ended unexpectedly. A %s was expected.",
                type);
        else
            reporter.error(
                ArcError.TokenExpectMismatch, span,
                "An unexpected token was encountered: Expected (%s), Encountered (%s)",
                type, tokens.source_text[span.start .. span.start + span.length]);
        return false;
    }

    void resynchronize() { while (indentation_level > 0 && !tokens.done) advance(); }
}

AstNode* parse_symbol(AstNode.Kind kind)(ParsingContext* p) {
    auto t = p.take();
    return p.nodes.alloc(kind, t.span, t.key);
}

AstNode* parse_unary(AstNode.Kind kind)(ParsingContext* p) {
    auto t = p.take();
    auto operand = parse_expression(p, Precedence.Call);

    if (operand.is_valid)
        return p.nodes.alloc(kind, t.span, operand);

    return operand.as_invalid(t.span);
}

AstNode* parse_binary(ParsingContext* p, AstNode* lhs, Infix op) {
    if (op.skip_token) p.advance();

    auto rhs = parse_expression(p, cast(Precedence) (op.prec + op.is_left_associative));
    if (rhs.is_valid)
        return p.nodes.alloc(op.kind, p.nodes.alloc_sequence(lhs, rhs));

    auto node = p.nodes.alloc(AstNode.Invalid, lhs.span.merge(rhs.span));
    p.nodes.free(lhs, rhs);
    return node;
}

AstNode* parse_expression(ParsingContext* p, Precedence prec = Precedence.Assign) {
    auto expr = prefixes[p.tokens.current.type](p);

    for (Infix i = infixes[p.tokens.current.type]; expr.is_valid && prec <= i.prec; i = infixes[p.tokens.current.type])
        expr = parse_binary(p, expr, i);

    if (expr.is_valid) return expr;

    p.nodes.free(expr.children);
    return expr.as_invalid(expr.span);
}

alias PrefixFn = AstNode* function(ParsingContext*);

immutable prefixes = () {
    PrefixFn[256] parsers   = (p) {
        p.reporter.error(
            ArcError.TokenNotAnExpression, p.tokens.current.span,
            "The token \"%s\" cannot start a prefix expression.",
            p.tokens.current.type);
        return p.nodes.alloc(AstNode.Kind.Invalid, p.take().span);
    };

    parsers[Token.Done]     = (p) {
        p.reporter.error(
            ArcError.UnexpectedEndOfFile, p.tokens.current.span,
            "Unexpected end of file while parsing source.");
        return p.nodes.alloc(AstNode.Kind.Invalid, p.tokens.current.span);
    };

    with (Token.Type) {
        parsers[Minus]      = &parse_unary!(AstNode.Negate);
        parsers[Bang]       = &parse_unary!(AstNode.Not);
        parsers[Not]        = &parse_unary!(AstNode.Not);

        parsers[Name]       = &parse_symbol!(AstNode.Name);
        parsers[Integer]    = &parse_symbol!(AstNode.Integer);
        parsers[Char]       = &parse_symbol!(AstNode.Char);

        // parsers[Lparen]       = &parse_list!(Rparen, false);
        // parsers[Lbracket]     = &parse_list!(Rbracket, false);
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
        // ops[Lparen]         = Infix(Call,       &parse_call!(Rparen));
        // ops[Lbracket]       = Infix(Call,       &parse_call!(Rparen));
    }

    return ops;
} ();
