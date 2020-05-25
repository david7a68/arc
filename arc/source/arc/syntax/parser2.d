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

AstNode* parse_symbol(ParsingContext* p) {
    auto t = p.take();
    import std.stdio; writeln("ps: ", token_map[t.type].kind);
    return p.nodes.alloc(token_map[t.type].kind, t.span, t.key);
}

AstNode* parse_unary(AstNode.Kind kind)(ParsingContext* p) {
    auto t = p.take();
    auto operand = parse_expression(p, Precedence.Call);

    if (operand.is_valid)
        return p.nodes.alloc(kind, t.span, operand);

    return operand.as_invalid(t.span);
}

// AstNode* parse_binary(Precedence prec)(ParsingContext* p, AstNode* lhs) {
//     auto op = token_map[p.current.type];
    
//     if (op.prec == Precedence.Call) {
//         auto rhs = parse_expression(p, op.prec)
//     }
//     else {
//         p.advance();
//         auto rhs = parse_expression(p, op.prec);

//         if (rhs.is_valid)
//             return alloc_binary(p.nodes, kind.type, lhs, rhs);

//         auto node = alloc_basic(AstNode.Invalid, lhs.span.merge(rhs.span));
//         p.nodes.free(lhs.index, rhs.index);
//         return node;
//     }
// }

AstNode* parse_expression(ParsingContext* p, Precedence prec = Precedence.Assign) {
    auto expr = prefixes[p.tokens.current.type](p);

    // while (expr.is_valid && prec < token_map[p.tokens.current.type].prec)
    //     expr = parse_binary(p, expr);

    if (expr.is_valid) return expr;

    scope (exit) p.nodes.free(expr);
    return p.nodes.alloc(AstNode.Kind.Invalid, expr.span);
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

        parsers[Name]       = &parse_symbol;
        parsers[Integer]    = &parse_symbol;
        parsers[Char]       = &parse_symbol;

        // parsers[Lparen]       = &parse_list!(Rparen, false);
        // parsers[Lbracket]     = &parse_list!(Rbracket, false);
    }

    return parsers;
} ();

struct MapItem { AstNode.Kind kind; Precedence next_prec; }

MapItem[256] token_map = () {
    MapItem[256] map;
    with (Token.Type) {
        // values (they can be used as a call in a binary expr, hence their precedence)
        map[Name]           = MapItem(AstNode.Name,         Precedence.Prefix);
        map[Integer]        = MapItem(AstNode.Integer,      Precedence.Prefix);
        map[Char]           = MapItem(AstNode.Char,         Precedence.Prefix);
        // unary operators (ditto)
        map[Minus]          = MapItem(AstNode.Negate,       Precedence.Prefix);
        map[Bang]           = MapItem(AstNode.Not,          Precedence.Prefix);
        map[Not]            = MapItem(AstNode.Not,          Precedence.Prefix);
        // binary operators
        map[Plus]           = MapItem(AstNode.Add,          cast(Precedence) (Precedence.Sum + 1));
        map[Minus]          = MapItem(AstNode.Subtract,     cast(Precedence) (Precedence.Sum + 1));
        map[Star]           = MapItem(AstNode.Multiply,     cast(Precedence) (Precedence.Product + 1));
        map[Slash]          = MapItem(AstNode.Divide,       cast(Precedence) (Precedence.Product + 1));
        map[Caret]          = MapItem(AstNode.Power,        cast(Precedence) (Precedence.Power)); // cast is a noop
        map[Less]           = MapItem(AstNode.Less,         cast(Precedence) (Precedence.Equality + 1));
        map[LessEqual]      = MapItem(AstNode.LessEqual,    cast(Precedence) (Precedence.Equality + 1));
        map[Greater]        = MapItem(AstNode.Greater,      cast(Precedence) (Precedence.Equality + 1));
        map[GreaterEqual]   = MapItem(AstNode.GreaterEqual, cast(Precedence) (Precedence.Equality + 1));
        map[Equals]         = MapItem(AstNode.Equal,        cast(Precedence) (Precedence.Equality + 1));
        map[BangEqual]      = MapItem(AstNode.NotEqual,     cast(Precedence) (Precedence.Equality + 1));
        map[And]            = MapItem(AstNode.And,          cast(Precedence) (Precedence.Logic + 1));
        map[Or]             = MapItem(AstNode.Or,           cast(Precedence) (Precedence.Logic + 1));
    }
    return map;
} ();
