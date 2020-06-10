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
        if (match_required(type)) return take();
        return Token();
    }

    bool match_required(Token.Type type) {
        if (type == tokens.current.type) return true;

        const span = tokens.current.span;
        ArcError.Code ecode;
        const(char)[] emsg;

        if (tokens.done) {
            if (!reporter.has_error(ArcError.UnexpectedEndOfFile)) {
                ecode = ArcError.UnexpectedEndOfFile,
                emsg = tprint("The file ended unexpectedly. A %s was expected.", type);
            }
        }
        else {
            ecode = ArcError.TokenExpectMismatch;
            emsg = tprint("An unexpected token was encountered: Expected (%s), Encountered (%s)",
                type, tokens.source_text[span.start .. span.start + span.length]);
        }
        reporter.error(ecode, span, emsg);
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
}

auto parse_optional_type(ParsingContext* p) {
    return p.current.type != Token.Equals ? parse_type(p) : AstNode.inferred;
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

AstNode* parse_assign(ParsingContext* p, AstNode* lhs) {
    p.skip_required(Token.Equals);

    auto rhs = parse_expression(p);
    const span = lhs.span.merge(rhs.span);

    if (rhs.is_valid)
        return p.alloc(AstNode.Assign, lhs, rhs);

    p.free(lhs);
    return rhs.respan(span);
}

AstNode* parse_block(ParsingContext* p) {
    auto start = p.take().span;

    auto seq = SequenceBuilder(p.nodes);
    while (!p.current.type.matches_one(Token.Done, Token.Rbrace)) {
        auto node = parse_statement(p);

        if (!node.is_valid) {
            seq.abort();
            return p.alloc(AstNode.Invalid, start.merge(node.span));
        }
        seq.add(node);
    }

    auto span = p.current.span.merge(start);
    if (p.skip_required(Token.Rbrace))
        return p.alloc(AstNode.Block, span, seq.nodes);

    seq.abort();
    return p.alloc(AstNode.Invalid, span);
}

AstNode* parse_if(ParsingContext* p) {
    auto start = p.take_required(Token.If).span;

    auto cond = parse_expression(p);
    auto body = parse_block(p);
    auto base = p.skip(Token.Else) ? parse_statement(p) : AstNode.none;
    auto span = merge_all(start, cond.span, body.span, base.span);

    if (cond.is_valid && body.is_valid && base.is_valid)
        return p.alloc(AstNode.If, span, p.make_seq(cond, body, base));

    p.free(cond, body, base);
    return p.alloc(AstNode.Invalid, span);
}

AstNode* parse_loop(ParsingContext* p) {
    auto start = p.take_required(Token.Loop).span;

    auto body = parse_block(p);

    if (body.is_valid) return p.alloc(AstNode.Loop, start, body);

    scope (exit) p.free(body);
    return p.alloc(AstNode.Invalid, start.merge(body.span));
}

AstNode* parse_escape(ParsingContext* p, Token.Type token, AstNode.Kind kind) {
    auto start = p.take_required(token).span;
    return p.alloc(kind, start);
}

AstNode* parse_return(ParsingContext* p) {
    auto start = p.take_required(Token.Return).span;

    auto expr = p.current.type != Token.Semicolon ? parse_expression(p) : AstNode.none;

    if (expr.is_valid)
        return p.alloc(AstNode.Return, start, expr);

    return expr.respan(expr.span.merge(start));
}

AstNode* parse_define(ParsingContext* p) {
    const start = p.take().span;
    auto name = parse_symbol(p, AstNode.Name);

    if (!p.skip_required(Token.Colon))
        return name.as_invalid(start.merge(name.span));

    auto type = parse_optional_type(p);
    auto expr = parse_optional_expr(p);
    auto span = merge_all(start, type.span, expr.span); // name must be between start and type

    if (type.is_valid && expr.is_valid)
        return p.alloc(AstNode.Definition, span, p.make_seq(name, type, expr));

    p.free(type, expr);
    return name.as_invalid(span);
}

AstNode* parse_variable(ParsingContext* p, AstNode* name) {
    p.skip_required(Token.Colon);

    auto type = parse_optional_type(p);
    auto expr = parse_optional_expr(p);
    auto span = merge_all(name.span, type.span, expr.span);

    if (type.is_valid && expr.is_valid)
        return p.alloc(AstNode.Variable, span, p.make_seq(name, type, expr));

    p.free(type, expr);
    return name.as_invalid(span);
}

AstNode* parse_statement(ParsingContext* p) {
    auto stmt = () {
        switch (p.current.type) with (Token.Type) {
            case Break:     return parse_escape(p, Token.Break, AstNode.Break);
            case Continue:  return parse_escape(p, Token.Continue, AstNode.Continue);
            case Def:       return parse_define(p);
            case If:        return parse_if(p);
            case Lbrace:    return parse_block(p);
            case Loop:      return parse_loop(p);
            case Return:    return parse_return(p);
            default:
                auto prefix = parse_prefix(p);

                if (!prefix.is_valid) return prefix;

                if (prefix.kind == AstNode.Kind.Name && p.current.type == Token.Colon)
                    return parse_variable(p, prefix);

                auto infix = parse_infix(p, prefix);

                if (p.current.type == Token.Equals)
                    infix = parse_assign(p, infix);

                return infix;
        }
    } ();

    if (stmt.kind == AstNode.Block || stmt.kind == AstNode.If || stmt.kind == AstNode.Loop) return stmt;

    if (stmt.is_valid) {
        auto semicolon = p.take_required(Token.Semicolon);
        if (semicolon.type == Token.Semicolon) {
            stmt.span = stmt.span.merge(semicolon.span);
            return stmt;
        }
        scope (exit) p.free(stmt);
        stmt = p.alloc(AstNode.Invalid, stmt.span);
    }

    return stmt;
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
    auto t = p.take();
    auto operand = parse_expression(p, Precedence.Call);
    auto span = t.span.merge(operand.span);

    if (operand.is_valid)
        return p.alloc(kind, span, operand);

    return operand.respan(span);
}

AstNode* parse_list(bool is_type)(ParsingContext* p, in Token.Type closing) {
    static parse_member(ParsingContext* p) {
        auto first = parse_expression(p, Precedence.Logic);

        if (!first.is_valid)
            return first.as_invalid(first.span);

        if (p.skip(Token.Colon)) {
            auto type = parse_optional_type(p);
            auto expr = parse_optional_expr(p);
            auto span = merge_all(first.span, type.span, expr.span);

            if (type.is_valid && expr.is_valid)
                return p.alloc(AstNode.Variable, span, p.make_seq(first, type, expr));

            p.free(first, type, expr);
            return p.alloc(AstNode.Invalid, span);
        }

        static if (is_type)
            auto parts = p.make_seq(AstNode.none, first, AstNode.inferred);
        else
            auto parts = p.make_seq(AstNode.none, AstNode.inferred, first);

        return p.alloc(AstNode.Variable, first.span, parts);
    }

    const begin = p.take().span;
    while (p.current.type == Token.Comma) p.advance();

    auto seq = SequenceBuilder(p.nodes);
    while (!p.current.type.matches_one(closing, Token.Done)) {
        auto node = parse_member(p);

        if (node.is_valid && (p.current.type.matches_one(closing, Token.Done) || p.skip_required(Token.Comma)))
            while (p.current.type == Token.Comma) p.advance();
        else {
            scope (exit) seq.abort();
            return p.alloc(AstNode.Invalid, begin.merge(node.span));
        }

        seq.add(node);
    }

    const end = p.current.span;
    if (!p.skip_required(closing)) {
        seq.abort();
        return p.alloc(AstNode.Invalid, begin.merge(end));
    }

    auto members = seq.nodes;
    auto list = p.alloc(AstNode.List, begin.merge(end), members);

    if (p.current.type != Token.Rarrow) return list;

    static if (is_type) return parse_function_type(p, list);
    else return parse_function(p, list);
}

AstNode* parse_import(ParsingContext* p) {
    const start = p.take_required(Token.Import).span;

    if (p.current.type != Token.Name) {
        p.reporter.error(ArcError.TokenExpectMismatch, start,
            "An import symbol must be followed by a path to the module to import.");
        
        return p.alloc(AstNode.Invalid, start);
    }

    auto name = parse_expression(p, Precedence.Call);
    return p.alloc(AstNode.Import, start, name);
}

AstNode* parse_binary(ParsingContext* p, AstNode* lhs, in Infix op) {
    if (op.skip_token) p.advance();

    auto rhs = parse_expression(p, cast(Precedence) (op.prec + op.is_left_associative));
    if (rhs.is_valid)
        return p.alloc(op.kind, lhs, rhs);

    scope (exit) p.free(lhs, rhs);
    return p.alloc(AstNode.Invalid, lhs.span.merge(rhs.span));
}

AstNode* parse_function(ParsingContext* p, AstNode* list) {
    p.advance();

    auto expr = p.current.type != Token.Lbrace ?
                parse_expression(p) :
                AstNode.inferred;

    auto body = p.current.type == Token.Lbrace ?
                parse_block(p) :
                expr;

    auto type = () @nogc { // handle () -> a, where expr is duplicated in body
        AstNode*[2] select = [expr, AstNode.inferred];
        return select[expr is body];
    } ();

    if (type.is_valid && body.is_valid)
        return p.alloc(AstNode.Function, p.make_seq(list, type, body));

    scope (exit) p.free(list, type, body);
    return p.alloc(AstNode.Invalid, merge_all(list.span, type.span, body.span));
}

AstNode* parse_expression(ParsingContext* p, in Precedence prec = Precedence.Assign) {
    auto expr = parse_prefix(p);
    return parse_infix(p, expr, prec);
}

AstNode* parse_prefix(ParsingContext* p) {
    ArcError.Code ecode;
    const(char)[] emsg;

    const token = p.current;
    switch (token.type) with (Token.Type) {
        case Minus:             return parse_unary(p, AstNode.Negate);
        case Bang: case Not:    return parse_unary(p, AstNode.Not);
        case Name:              return parse_symbol(p, AstNode.Name);
        case Integer:           return parse_symbol(p, AstNode.Integer);
        case Char:              return parse_symbol(p, AstNode.Char);
        case Import:            return parse_import(p);
        case Lparen:            return parse_list!false(p, Rparen);
        case Lbracket:          return parse_list!false(p, Rbracket);

        case Done:
            ecode = ArcError.UnexpectedEndOfFile;
            emsg = "Unexpected end of file while parsing prefix expression.";
            break;

        default:
            if (token.type.matches_one(Semicolon, Comma, Rparen, Rbracket, Rbrace)) {
                ecode = ArcError.TokenExpectMismatch;
                emsg = tprint("Unexpected end of expression by terminating %s.", token.type);
            }
            else {
                ecode = ArcError.TokenExpectMismatch;
                emsg = tprint("The token \"%s\" cannot start a prefix expression.", token.type);
                p.advance();
            }
    }

    p.reporter.error(ecode, token.span, emsg);
    return p.alloc(AstNode.Kind.Invalid, token.span);
}

AstNode* parse_infix(ParsingContext* p, AstNode* expr, in Precedence prec = Precedence.Assign) {
    static immutable Infix[256] infixes = [
        Token.Less           : Infix(Precedence.Compare,    true,   true,   AstNode.Less),
        Token.LessEqual      : Infix(Precedence.Compare,    true,   true,   AstNode.LessEqual),
        Token.Greater        : Infix(Precedence.Compare,    true,   true,   AstNode.Greater),
        Token.GreaterEqual   : Infix(Precedence.Compare,    true,   true,   AstNode.GreaterEqual),
        Token.EqualEqual     : Infix(Precedence.Equality,   true,   true,   AstNode.Equal),
        Token.BangEqual      : Infix(Precedence.Equality,   true,   true,   AstNode.NotEqual),
        Token.And            : Infix(Precedence.Logic,      true,   true,   AstNode.And),
        Token.Or             : Infix(Precedence.Logic,      true,   true,   AstNode.Or),
        Token.Plus           : Infix(Precedence.Sum,        true,   true,   AstNode.Add),
        Token.Minus          : Infix(Precedence.Sum,        true,   true,   AstNode.Subtract),
        Token.Star           : Infix(Precedence.Product,    true,   true,   AstNode.Multiply),
        Token.Slash          : Infix(Precedence.Product,    true,   true,   AstNode.Divide),
        Token.Caret          : Infix(Precedence.Power,      false,  true,   AstNode.Power),
        Token.Dot            : Infix(Precedence.Call,       true,   true,   AstNode.Access),
        Token.ColonColon     : Infix(Precedence.Call,       true,   true,   AstNode.StaticAccess),
        Token.Name           : Infix(Precedence.Call,       true,   false,  AstNode.Call),
        Token.Integer        : Infix(Precedence.Call,       true,   false,  AstNode.Call),
        Token.Char           : Infix(Precedence.Call,       true,   false,  AstNode.Call),
        Token.Lparen         : Infix(Precedence.Call,       true,   false,  AstNode.Call),
        Token.Lbracket       : Infix(Precedence.Call,       true,   false,  AstNode.Call)
    ];

    for (Infix i = infixes[p.current.type]; expr.is_valid && prec <= i.prec; i = infixes[p.current.type])
        expr = parse_binary(p, expr, i);

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

    auto type = parse_type(p);

    if (type.is_valid)
        return p.alloc(AstNode.FunctionType, list, type);

    scope (exit) { p.free(list); p.free(type.children); }
    return type.as_invalid(list.span.merge(type.span));
}

AstNode* parse_type(ParsingContext* p) {
    auto prefix = parse_type_prefix(p);
    return parse_type_infix(p, prefix);
}

AstNode* parse_type_prefix(ParsingContext* p) {
    switch (p.current.type) with (Token.Type) {
        case Star:
            const start = p.take().span;
            return p.alloc(AstNode.PointerType, start, parse_type(p));

        case Name:      return parse_symbol(p, AstNode.Name);
        case Lparen:    return parse_list!true(p, Token.Rparen);
        case Lbracket:  return parse_list!true(p, Token.Rbracket);
        default:
            p.reporter.error(
                ArcError.TokenExpectMismatch, p.current.span,
                "A type expression may be an access, symbol, list, function type, or call, not a %s", p.current.type
            );

            return p.alloc(AstNode.Invalid, p.take().span);
    }
}

AstNode* parse_type_infix(ParsingContext* p, AstNode* expr) {
    static immutable Infix[256] ops = [
        Token.Dot       : Infix(Precedence.Call, true, true,    AstNode.Access),
        Token.Lparen    : Infix(Precedence.Call, true, false,   AstNode.Call),
        Token.Lbracket  : Infix(Precedence.Call, true, false,   AstNode.Call)
    ];

    for (Infix i = ops[p.current.type]; expr.is_valid && Precedence.Call <= i.prec; i = ops[p.current.type])
        expr = parse_binary(p, expr, i);

    return expr;
}
