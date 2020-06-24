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
import arc.data.span;
import arc.reporter;
import arc.syntax.lexer : matches_one, Token, TokenBuffer;
import arc.syntax.syntax_allocator;

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
    SyntaxAllocator nodes;
    Reporter* reporter;

    int indentation_level;

    alias nodes this;

    this(Reporter* reporter, SyntaxAllocator node_allocator) {
        this.reporter = reporter;
        this.nodes = node_allocator;
    }

    void begin(const char[] source_text) {
        tokens.begin(source_text);
        reporter.clear();
        indentation_level = tokens.current.type == Token.Type.Lbrace ? 1 : 0;
    }

    bool is_done() {
        return tokens.current.type == Token.Type.Done;
    }

    Token token() {
        return tokens.current;
    }

    void advance() {
        tokens.advance();

        if (tokens.current.type == Token.Type.Lbrace)
            indentation_level++;
        else if (tokens.current.type == Token.Type.Rbrace)
            indentation_level--;
    }

    Token take() {
        scope (exit)
            advance();
        return tokens.current;
    }

    bool skip(Token.Type type) {
        return skip(type == tokens.current.type);
    }

    bool skip_required(Token.Type type) {
        return skip(match_required(type));
    }

    bool skip(bool should_skip) {
        if (should_skip)
            advance();
        return should_skip;
    }

    Token take_required(Token.Type type) {
        return match_required(type) ? take() : Token();
    }

    bool match_required(Token.Type type) {
        if (type == tokens.current.type)
            return true;

        const span = tokens.current.span;
        if (tokens.done)
            reporter.error(ArcError.UnexpectedEOF, span,
                    "The file ended unexpectedly. A %s was expected.", type);
        else
            reporter.error(ArcError.TokenExpectMismatch, span,
                    "An unexpected token was encountered: Expected (%s), Encountered (%s)",
                    type, tokens.source_text[span.start .. span.start + span.length]);
        return false;
    }

    Token resynchronize() {
        if (indentation_level == 0)
            return Token();

        while (tokens.current.type != Token.Type.Done) {
            if (indentation_level == 0)
                break;
            advance();
        }

        auto close = take();
        return tokens.current.type == Token.Type.Semicolon ? take() : close;
    }

    AstNode* make_unary(AstNode.Kind kind, Span prefix, AstNode* node) {
        auto span = prefix + node.span;

        if (node.is_valid)
            return nodes.alloc_ast(kind, span, node);

        nodes.free(node);
        return make_invalid(span);
    }

    AstNode* make_n_ary_prefix(AstNode.Kind kind, Span prefix, AstNode*[] seq...)
    in(seq.length > 1) {
        return make_seq(kind, prefix + merge_spans(seq), nodes.make_seq(seq));
    }

    AstNode* make_n_ary(AstNode.Kind kind, AstNode*[] seq...)
    in(seq.length > 1) {
        return make_seq(kind, merge_spans(seq), nodes.make_seq(seq));
    }

    AstNode* make_seq(AstNode.Kind kind, Span span, AstNode*[] seq) {
        if (seq.is_valid)
            return nodes.alloc_ast(kind, span, seq);

        nodes.free(seq);
        return make_invalid(span);
    }

    AstNode* make_invalid(Span span) {
        return nodes.alloc_ast(AstNode.Kind.Invalid, span);
    }
}

Span merge_spans(AstNode*[] nodes...) {
    import std.algorithm: map, fold;

    return nodes.map!(n => n.span).fold!((a, b) => (a + b));
}

struct SequenceNodeInfo {
    AstNode.Kind kind;
    Token.Type open;
    Token.Type close;
    Token.Type delim = Token.Type.None;
}

AstNode* parse_seq(alias parse_member)(ParsingContext* p, SequenceNodeInfo info) {
    auto start = p.take_required(info.open).span;

    if (start.length == 0)
        return p.make_invalid(start);

    auto seq = p.nodes.get_ast_appender();
    while (!p.token.type.matches_one(Token.Type.Done, info.close)) with (info) {
        auto node = parse_member(p);

        const bad_delim = delim && !p.token.type.matches_one(Token.Type.Done, close) && !p.skip_required(delim);
        if (!node.is_valid || bad_delim) {
            scope (exit)
                seq.abort();
            return p.make_invalid(start + node.span);
        }

        while (p.token.type == delim)
            p.advance();

        seq ~= node;
    }

    auto span = p.token.span + start;
    if (p.skip_required(info.close))
        return p.alloc_ast(info.kind, span, seq.get());

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
    p.skip_required(Token.Type.Equals);
    auto rhs = expr(p);
    return p.make_n_ary(AstNode.Kind.Assign, lhs, rhs);
}

immutable block_sequence = SequenceNodeInfo(AstNode.Kind.Block, Token.Type.Lbrace, Token.Type.Rbrace);

AstNode* if_(ParsingContext* p) {
    auto start = p.take_required(Token.Type.If).span;
    auto cond = expr(p);
    auto body = parse_seq!stmt(p, block_sequence);
    auto base = p.skip(Token.Type.Else) ? stmt(p) : AstNode.none;
    return p.make_n_ary_prefix(AstNode.Kind.If, start, cond, body, base);
}

AstNode* loop(ParsingContext* p) {
    auto start = p.take_required(Token.Type.Loop).span;
    auto body = parse_seq!stmt(p, block_sequence);
    return p.make_unary(AstNode.Kind.Loop, start, body);
}

AstNode* escape(ParsingContext* p, Token.Type token, AstNode.Kind kind) {
    return p.alloc_ast(kind, p.take_required(token).span);
}

AstNode* return_(ParsingContext* p) {
    auto start = p.take_required(Token.Type.Return).span;
    auto value = p.token.type != Token.Type.Semicolon ? expr(p) : AstNode.none;
    return p.make_unary(AstNode.Kind.Return, start, value);
}

AstNode* define(ParsingContext* p) {
    const start = p.take().span;
    auto name = p.alloc_ast(AstNode.Kind.Name, p.token.span, p.take.key);

    if (!p.skip_required(Token.Type.Colon))
        return name.as_invalid(start + name.span);

    auto node = decl(p, AstNode.Kind.Definition, name);
    node.span += start;
    return node;
}

AstNode* variable(ParsingContext* p, AstNode* name) {
    p.skip_required(Token.Type.Colon);
    return decl(p, AstNode.Kind.Variable, name);
}

AstNode* decl(ParsingContext* p, AstNode.Kind kind, AstNode* name) {
    auto type = p.token.type != Token.Type.Equals ? type(p) : AstNode.inferred;
    auto expr = p.skip(Token.Type.Equals) ? expr(p, Precedence.Logic) : AstNode.inferred;
    return p.make_n_ary(kind, name, type, expr);
}

AstNode* stmt(ParsingContext* p) {
    auto stmt = () {
        switch (p.token.type) with (Token.Type) {
            // dfmt off
        case If:        return if_(p);
        case Loop:      return loop(p);
        case Return:    return return_(p);
        case Def:       return define(p);
        case Break:     return escape(p, Token.Type.Break, AstNode.Kind.Break);
        case Continue:  return escape(p, Token.Type.Continue, AstNode.Kind.Continue);
        case Lbrace:    return parse_seq!stmt(p, block_sequence);
        // dfmt on
        default:
            auto prefix = prefix_expr(p);

            if (prefix.kind == AstNode.Kind.Name && p.token.type == Token.Type.Colon)
                return variable(p, prefix);

            auto infix = infix(p, prefix);
            return p.token.type == Token.Type.Equals ? parse_assign(p, infix) : infix;
        }
    }();

    switch (stmt.kind) with (AstNode.Kind) {
    case Block:
    case If:
    case Loop:
    case Invalid:
        return stmt;

    default:
        const semicolon = p.take_required(Token.Type.Semicolon);
        if (semicolon.type == Token.Type.Semicolon) {
            stmt.span += semicolon.span;
            return stmt;
        }
        scope (exit)
            p.free(stmt);
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

AstNode* list(bool is_type)(ParsingContext* p, Token.Type open, Token.Type close) {
    static parse_member(bool is_type)(ParsingContext* p) {
        auto first = expr(p, Precedence.Logic);
        if (p.skip(Token.Type.Colon))
            return decl(p, AstNode.Kind.ListMember, first);

        static if (is_type)
            return p.make_n_ary(AstNode.Kind.ListMember, AstNode.none, first, AstNode.inferred);
        else
            return p.make_n_ary(AstNode.Kind.ListMember, AstNode.none, AstNode.inferred, first);
    }

    auto list = parse_seq!(parse_member!is_type)(p,
            SequenceNodeInfo(AstNode.Kind.List, open, close, Token.Type.Comma));

    if (p.token.type != Token.Type.Rarrow)
        return list;
    else if (is_type)
        return function_type(p, list);
    else
        return function_(p, list);
}

AstNode* binary(alias parse_fn)(ParsingContext* p, AstNode* lhs, in Infix op) {
    p.skip(op.skip_token);
    auto rhs = parse_fn(p, cast(Precedence)(op.prec + op.is_left_associative));
    return p.make_n_ary(op.kind, lhs, rhs);
}

AstNode* function_(ParsingContext* p, AstNode* list) {
    p.advance();
    auto first = p.token.type == Token.Type.Lbrace ? AstNode.inferred : expr(p);
    // dfmt off
    return p.token.type == Token.Type.Lbrace
           ? p.make_n_ary(AstNode.Kind.Function, list, first, parse_seq!stmt(p, block_sequence))
           : p.make_n_ary(AstNode.Kind.Function, list, AstNode.inferred, first);
    // dfmt on
}

AstNode* expr(ParsingContext* p, in Precedence prec = Precedence.Assign) {
    return infix(p, prefix_expr(p), prec);
}

AstNode* prefix_expr(ParsingContext* p) {
    const token = p.token;
    // dfmt off
    switch (token.type) with (Token.Type) {
    case Bang: case Not:    return p.make_unary(AstNode.Kind.Not, p.take().span, expr(p, Precedence.Call));
    case Minus:             return p.make_unary(AstNode.Kind.Negate, p.take().span, expr(p, Precedence.Call));
    case Import:            return p.make_unary(AstNode.Kind.Import, p.take().span, expr(p, Precedence.Call));
    case Name:              return p.alloc_ast(AstNode.Kind.Name, p.token.span, p.take.key);
    case Integer:           return p.alloc_ast(AstNode.Kind.Integer, p.token.span, p.take.key);
    case Char:              return p.alloc_ast(AstNode.Kind.Char, p.token.span, p.take.key);
    case String:            return p.alloc_ast(AstNode.Kind.String, p.token.span, p.take.key);
    case Lparen:            return list!false(p, Lparen, Rparen);
    case Lbracket:          return list!false(p, Lbracket, Rbracket);
    default:
    }
    // dfmt on

    if (token.type == Token.Type.Done)
        p.reporter.error(ArcError.UnexpectedEOF, token.span,
                "Unexpected end of file while parsing expression.");
    else
        with (Token.Type) {
            auto s = token.type.matches_one(Semicolon, Comma, Rparen, Rbracket,
                    Rbrace) ? tprint("Expression ended unexpectedly with a %s.",
                    token.type) : tprint("The token \"%s\" cannot start a prefix expression.",
                    token.type);
            p.reporter.error(ArcError.TokenExpectMismatch, token.span, s);
        }

    return p.make_invalid(token.span);
}

AstNode* infix(ParsingContext* p, AstNode* lhs, in Precedence prec = Precedence.Assign) {
    // dfmt off
    with (Token.Type) {
        static immutable Infix[256] infixes = [
            Less          : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.Less),
            LessEqual     : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.LessEqual),
            Greater       : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.Greater),
            GreaterEqual  : Infix(Precedence.Compare,     true,   true,   AstNode.Kind.GreaterEqual),
            EqualEqual    : Infix(Precedence.Equality,    true,   true,   AstNode.Kind.Equal),
            BangEqual     : Infix(Precedence.Equality,    true,   true,   AstNode.Kind.NotEqual),
            And           : Infix(Precedence.Logic,       true,   true,   AstNode.Kind.And),
            Or            : Infix(Precedence.Logic,       true,   true,   AstNode.Kind.Or),
            Plus          : Infix(Precedence.Sum,         true,   true,   AstNode.Kind.Add),
            Minus         : Infix(Precedence.Sum,         true,   true,   AstNode.Kind.Subtract),
            Star          : Infix(Precedence.Product,     true,   true,   AstNode.Kind.Multiply),
            Slash         : Infix(Precedence.Product,     true,   true,   AstNode.Kind.Divide),
            Caret         : Infix(Precedence.Power,       false,  true,   AstNode.Kind.Power),
            Dot           : Infix(Precedence.Call,        true,   true,   AstNode.Kind.Access),
            ColonColon    : Infix(Precedence.Call,        true,   true,   AstNode.Kind.StaticAccess),
            Name          : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
            Integer       : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
            Char          : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
            String        : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
            Lparen        : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call),
            Lbracket      : Infix(Precedence.Call,        true,   false,  AstNode.Kind.Call)
        ];
        //dfmt on

        return infix!expr(p, lhs, prec, infixes[]);
    }
}

AstNode* infix(alias fn)(ParsingContext* p, AstNode* lhs, Precedence prec, in Infix[] ops) {
    for (Infix i = ops[p.token.type]; lhs.is_valid && prec <= i.prec; i = ops[p.token.type])
        lhs = binary!fn(p, lhs, i);
    return lhs;
}

struct Infix {
    Precedence prec;
    bool is_left_associative, skip_token;
    AstNode.Kind kind;
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

AstNode* function_type(ParsingContext* p, AstNode* list) {
    p.skip_required(Token.Type.Rarrow);
    return p.make_n_ary(AstNode.Kind.FunctionType, list, type(p));
}

AstNode* type(ParsingContext* p, Precedence prec = Precedence.Assign) {
    return type_infix(p, type_prefix(p), prec);
}

AstNode* type_prefix(ParsingContext* p) {
    switch (p.token.type) with (Token.Type) {
    // dfmt off
    case Star:      return p.make_unary(AstNode.Kind.PointerType, p.take().span, type(p));
    case Name:      return p.alloc_ast(AstNode.Kind.Name, p.token.span, p.take.key);
    case Lparen:    return list!true(p, Lparen, Rparen);
    case Lbracket:  return list!true(p, Lbracket, Rbracket);
    default:
    // dfmt on
    }

    p.reporter.error(ArcError.TokenExpectMismatch, p.token.span,
            "A type expression may be an access, symbol, list, function type, or call, not a %s",
            p.token.type);
    return p.make_invalid(p.take().span);
}

AstNode* type_infix(ParsingContext* p, AstNode* expr, Precedence prec) {
    // dfmt off
    static immutable Infix[256] ops = [
        Token.Type.Dot       : Infix(Precedence.Call, true, true, AstNode.Kind.Access),
        Token.Type.Lparen    : Infix(Precedence.Call, true, false, AstNode.Kind.Call),
        Token.Type.Lbracket  : Infix(Precedence.Call, true, false, AstNode.Kind.Call)
    ];
    // dfmt on

    return infix!type(p, expr, prec, ops);
}
