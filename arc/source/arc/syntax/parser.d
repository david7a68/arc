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
module arc.syntax.parser;

version (none):

import arc.data.ast;
import arc.data.source: Span, merge, merge_all;
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

final class Parser {
    TokenBuffer!2048 buffer;
    alias buffer this;

    Reporter* reporter;

    this(Reporter* reporter) {
        this.reporter = reporter;
    }

    void reset(const(char)[] text) {
        buffer = TokenBuffer!2048(text);
    }

    AstNode[] parse_text(const(char)[] text) {
        reset(text);
        
        AstNode[] result;
        while (!done)
            result ~= parse_statement(this);

        return result;
    }

    void drop_all(Token.Type type) {
        while (current.type != Token.Done && current.type == type)
            advance();
    }

    Token take() {
        auto token = current;
        advance();
        return token;
    }

    bool skip(Token.Type type) {
        if (current.type != type)
            return false;
        
        advance();
        return true;
    }

    bool skip_required_delim(Token.Type type) {
        return take_required_delim(type).type == type;
    }

    Token take_required_delim(Token.Type type) {
        if (current.type == type)
            return take();
        else {
            if (done)
                reporter.error(
                    ArcError.UnexpectedEndOfFile,
                    current.span,
                    "The file ended unexpectedly. A %s was expected.",
                    type
                );
            else
                reporter.error(
                    ArcError.TokenExpectMismatch,
                    current.span,
                    "An unexpected token was encountered: Expected (%s), Encountered (%s)",
                    type, source_text[current.span.start .. current.span.start + current.span.length]
                );

            return Token();
        }
    }

    void sync_to_semicolon() {
        while (!done && !current.type.matches_one(Token.Semicolon))
            advance();
    }

    T alloc(T, Args...)(Args args) {
        return new T(args);
    }

    void free(AstNode[] nodes...) {
        foreach (node; nodes)
            if (node.kind != AstNode.Kind.None && node.kind != AstNode.Kind.Inferred) {
                free(node.children);
                destroy(node);
            }
    }
}

AstNode parse_statement(Parser p) {
    switch (p.current.type) {
        case Token.Def:
            return parse_define(p);

        case Token.Lbrace:
            return parse_block(p);

        default:
            auto prefix = prefixes[p.current.type](p);

            if (prefix.kind == AstNode.Kind.Name && p.current.type == Token.Colon)
                return continue_variable(p, prefix);
            else {
                auto expr = prefix.is_valid ? parse_infix_expression(p, prefix) : prefix;

                if (expr.is_valid && p.skip_required_delim(Token.Semicolon))
                    return expr;
                else {
                    p.sync_to_semicolon();
                    
                    const span = expr.span.merge(p.take_required_delim(Token.Semicolon).span);
                    p.free(expr);

                    return p.alloc!Invalid(span);
                }
            }
    }
}

AstNode parse_expression(Parser p, Precedence prec = Precedence.Assign) {
    auto expr = prefixes[p.current.type](p);

    if (!expr.is_valid) {
        scope(exit) p.free(expr);
        return p.alloc!Invalid(expr.span);
    }

    return parse_infix_expression(p, expr, prec);
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

AstNode parse_define(Parser p) {
    const start = p.take().span;

    auto name = parse_value!Name(p);

    if (!p.skip_required_delim(Token.Colon)) {
        p.free(name);
        p.sync_to_semicolon();
        return p.alloc!Invalid(start.merge(p.take().span));
    }

    auto type = parse_optional_type(p);
    auto value = parse_optional_value(p);

    const semicolon = p.take_required_delim(Token.Semicolon);
    const span = start.merge(semicolon.span);

    if (semicolon.type == Token.Semicolon) {
        if (value.kind == AstNode.Kind.Inferred)
            return p.alloc!TypeDeclaration(span, name, type);
        else
            return p.alloc!ConstantDeclaration(span, name, type, value);
    }
    else {
        p.free(name, type, value);
        p.sync_to_semicolon();
        return p.alloc!Invalid(span);
    }
}

AstNode continue_variable(Parser p, AstNode name) {
    p.advance();

    auto type = parse_optional_type(p);
    auto expr = parse_optional_value(p);

    const semicolon = p.take_required_delim(Token.Semicolon);
    const span = name.span.merge(semicolon.span);

    if (semicolon.type == Token.Semicolon)
        return p.alloc!Variable(span, name, type, expr);
    else {
        p.free(name, type, expr);
        p.sync_to_semicolon();
        return p.alloc!Invalid(span);
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

alias PrefixParseFn = AstNode function(Parser);

immutable prefixes = () {
    PrefixParseFn[256] parsers  = (p) {
        p.reporter.error(
            ArcError.TokenNotAnExpression,
            p.current.span,
            "The token \"%s\" cannot start a prefix expression.",
            p.current.type
        );

        return p.alloc!Invalid(p.take().span);
    };

    parsers[Token.Done]         = (p) {
        p.reporter.error(
            ArcError.UnexpectedEndOfFile,
            p.current.span,
            "Unexpected end of file while parsing source."
        );

        return p.alloc!Invalid(p.current.span);
    };

    with (Token.Type) {
        foreach (t; [Minus, Bang])
            parsers[t] = &parse_unary;

        parsers[Name]         = &parse_value!(arc.data.ast.Name);
        parsers[Integer]      = &parse_value!(arc.data.ast.Integer);
        parsers[Char]         = &parse_value!(arc.data.ast.Char);
        parsers[Lparen]       = &parse_list!(Rparen, false);
        parsers[Lbracket]     = &parse_list!(Rbracket, false);
    }

    return parsers;
} ();

alias InfixParseFn = AstNode function(Parser, AstNode);
struct Infix { Precedence prec; InfixParseFn parser; }

immutable infixes = () {
    Infix[256] parsers;

    with (Token.Type) with (Precedence) {
        parsers[Equals]         = Infix(Assign,     &parse_binary!(Assign + 1));
        parsers[Less]           = Infix(Compare,    &parse_binary!(Compare + 1));
        parsers[LessEqual]      = Infix(Compare,    &parse_binary!(Compare + 1));
        parsers[Greater]        = Infix(Compare,    &parse_binary!(Compare + 1));
        parsers[GreaterEqual]   = Infix(Compare,    &parse_binary!(Compare + 1));
        parsers[EqualEqual]     = Infix(Equality,   &parse_binary!(Equality + 1));
        parsers[BangEqual]      = Infix(Equality,   &parse_binary!(Equality + 1));
        parsers[And]            = Infix(Logic,      &parse_binary!(Logic + 1));
        parsers[Or]             = Infix(Logic,      &parse_binary!(Logic + 1));
        parsers[Plus]           = Infix(Sum,        &parse_binary!(Sum + 1));
        parsers[Minus]          = Infix(Sum,        &parse_binary!(Sum + 1));
        parsers[Star]           = Infix(Product,    &parse_binary!(Product + 1));
        parsers[Slash]          = Infix(Product,    &parse_binary!(Product + 1));
        parsers[Caret]          = Infix(Power,      &parse_binary!(Power));
        parsers[Dot]            = Infix(Call,       &parse_binary!(Call + 1));

        parsers[Lparen]         = Infix(Call,       &parse_call!(Rparen));
        parsers[Lbracket]       = Infix(Call,       &parse_call!(Rparen));
    }

    return parsers;
} ();

AstNode parse_infix_expression(Parser p, AstNode prefix, Precedence prec = Precedence.Assign) {
    auto expr = prefix;

    while (expr.is_valid && prec <= infixes[p.current.type].prec)
        expr = infixes[p.current.type].parser(p, expr);

    if (expr.is_valid)
        return expr;

    scope (exit) p.free(expr);
    return p.alloc!Invalid(expr.span);
}

AstNode parse_binary(int precedence)(Parser p, AstNode lhs) in (lhs.is_valid) {
    auto binary = p.alloc!Binary(parse_value!Name(p), lhs, parse_expression(p, cast(Precedence) precedence));

    if (binary.operands[2].is_valid)
        return binary;

    scope (exit) p.free(binary);
    return p.alloc!Invalid(binary.span);
}

AstNode parse_block(Parser p) {
    auto start = p.take().span;

    AstNode[] statements;
    while (!p.done && p.current.type != Token.Rbrace) {
        statements ~= parse_statement(p);

        if (!statements[$ - 1].is_valid) {
            p.free(statements);
            return p.alloc!Invalid(start);
        }
    }
    
    auto end = p.current.span;
    if (p.skip_required_delim(Token.Rbrace))
        return p.alloc!Block(start.merge(end), statements);

    p.free(statements);
    return p.alloc!Invalid(start.merge(end));
}

AstNode parse_call(Token.Type closing_delim)(Parser p, AstNode lhs) in (lhs.is_valid) {
    auto args = parse_list!(closing_delim, false)(p);

    const span = args.span.merge(lhs.span);
    if (args.is_valid)
        return p.alloc!Call(lhs, args);

    p.free(lhs, args);
    return p.alloc!Invalid(span);
}

AstNode parse_function(Parser p, AstNode params) in (params.is_valid) {
    if (params.kind == AstNode.Kind.Name) { // convert name to (name)
        params = p.alloc!List(params.span, [cast(AstNode) p.alloc!Variable(params.span, AstNode.none, AstNode.inferred, params)]);
    }

    p.advance();

    if (p.current.type == Token.Lbrace) {
        auto body = parse_block(p);
        return p.alloc!Function(params.span.merge(body.span), params, AstNode.inferred, body);
    }

    auto expr = parse_expression(p);

    if (p.current.type == Token.Lbrace) {
        auto body = parse_block(p);
        return p.alloc!Function(params.span.merge(body.span), params, expr, body);
    }

    return p.alloc!Function(params.span.merge(expr.span), params, AstNode.inferred, expr);
}

AstNode parse_list(Token.Type closing_delim, bool parsing_typelist)(Parser p) {
    const open = p.take().span;
    p.drop_all(Token.Comma);

    AstNode[] members;
    if (!p.current.type.matches_one(closing_delim, Token.Done))
        while (true) {
            // here, we rely on parse_list_member to catch other invalid tokens
            members ~= parse_list_member!parsing_typelist(p);

            if (!members[$-1].is_valid) {
                p.free(members);
                return p.alloc!Invalid(open);
            }
            else if (p.current.type.matches_one(closing_delim, Token.Done)) {
                break;
            }
            else {
                if (p.skip_required_delim(Token.Comma)) {
                    p.drop_all(Token.Comma);

                    if (p.current.type.matches_one(closing_delim, Token.Done))
                        break;
                }
                else { // e.g: (a a)
                    p.free(members);
                    return p.alloc!Invalid(open);
                }
            }
        }

    const close_token = p.current();

    if (p.skip_required_delim(closing_delim))
        return p.alloc!List(open.merge(close_token.span), members);
    else {
        p.free(members);
        return p.alloc!Invalid(open.merge(close_token.span));
    }
}

/**
 parse_list_member is used for both type lists and regular lists. Type lists
 default expression-only members to be the type of the member (as in structs).
 To parse a type list, pass false to `prefer_expr`
 */
AstNode parse_list_member(bool parsing_typelist)(Parser p) {
    auto first = parse_expression(p, Precedence.Logic);

    if (!first.is_valid) {
        scope (exit) p.free(first);
        return p.alloc!Invalid(first.span);
    }

    Span span = first.span;
    if (p.skip(Token.Colon)) {
        auto type = parse_optional_type(p);
        auto expr = parse_optional_value(p);
        span = merge_all(span, type.span, expr.span);

        if (first.kind == AstNode.Kind.Name && type.is_valid && expr.is_valid)
            return p.alloc!Variable(span, first, type, expr);

        p.free(type, expr);     // first is freed in the first.is_valid check
        return p.alloc!Invalid(span);
    }
    else {
        static if (parsing_typelist)
            return p.alloc!Variable(span, AstNode.none, first, AstNode.inferred);
        else
            return p.alloc!Variable(span, AstNode.none, AstNode.inferred, first);
    }
}

AstNode parse_unary(Parser p) {
    auto node = p.alloc!Unary(parse_value!Name(p), parse_expression(p, Precedence.Call));

    if (node.operands[0].is_valid)
        return node;

    scope (exit) p.free(node);
    return p.alloc!Invalid(node.span);
}

AstNode parse_value(T)(Parser p) {
    auto t = p.take();
    return p.alloc!T(t.span, t.key);
}

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

AstNode parse_type_expr(Parser p) {
    static prefix(Parser p) {
        switch (p.current.type) {
            case Token.Name:        return parse_value!Name(p);
            case Token.Lparen:      return parse_list!(Token.Rparen, true)(p);
            case Token.Lbracket:    return parse_list!(Token.Rbracket, true)(p);
            default:
                p.reporter.error(
                    ArcError.TokenExpectMismatch,
                    p.current.span,
                    "A type expression must start with a name or a list, not a %s.",
                    p.current.type
                );

                return p.alloc!Invalid(p.take().span);
        }
    }

    static infix_parser_for(Token.Type t) {
        switch (t) with (Token.Type) {
            case Token.Dot:
                return Infix(Precedence.Call, &parse_binary!(Precedence.Call + 1));
            // case Token.Rarrow:
            default:
                return Infix();
        }
    }

    auto expr = prefix(p);
    while (expr.is_valid && Precedence.Call <= infix_parser_for(p.current.type).prec)
        expr = infix_parser_for(p.current.type).parser(p, expr);

    if (expr.is_valid)
        return expr;
    
    scope(exit) p.free(expr);
    return p.alloc!Invalid(expr.span.merge(p.current.span));
}

auto parse_optional_type(Parser p) {
    return p.current.type != Token.Equals ? parse_type_expr(p) : AstNode.inferred;
}

auto parse_optional_value(Parser p) {
    return p.skip(Token.Equals) ? parse_expression(p, Precedence.Logic) : AstNode.inferred;
}
