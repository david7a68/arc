module arc.syntax.parser;

import arc.hash: Key;
import arc.syntax.ast;
import arc.syntax.lexer: Lexer, Token, matches_one;
import arc.syntax.location:  Source;
import arc.syntax.error: SyntaxError;

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
    Primary
}

struct ParseCtx {
    Lexer tokens;
    Source source_text;
    SyntaxError[] errors;

    this(Source source) {
        tokens = Lexer(source.span);
        source_text = source;
    }

    void free(AstNode[] nodes...) {
        import std.algorithm: filter;

        foreach (node; nodes.filter!(a => !a.is_marker)) {
            free(node.get_children());
            destroy(node);
        }
    }

    /// Constructs an array from a comma-separated list of nodes
    AstNode[] node_array(AstNode[] nodes...) {
        return nodes.dup;
    }
}

AstNode parse_module(ref ParseCtx ctx) {
    ctx.tokens.push_eol_delimiter(Token.Semicolon);

    AstNode[] statements;
    while (!ctx.tokens.empty)
        statements ~= parse_statement(ctx);

    ctx.tokens.pop_eol_delimiter();

    return new AstNode(AstNode.Module, ctx.tokens.span, statements);
}

AstNode parse_statement(ref ParseCtx ctx) {
    switch (ctx.tokens.current.type) {
        case Token.Def:
            return parse_def(ctx);

        case Token.Break:
            return parse_escape!(AstNode.Break, Token.Break, false)(ctx);
        
        case Token.Return:
            return parse_escape!(AstNode.Return, Token.Return, true)(ctx);
        
        case Token.Continue:
            return parse_escape!(AstNode.Continue, Token.Continue, false)(ctx);
        
        case Token.If:
            return parse_if(ctx);
        
        case Token.Loop:
            return parse_loop(ctx);
        
        default:
            auto expression = parse_expression(ctx);
            ctx.tokens.skip_required(Token.Semicolon, ctx);

            return expression;
    }
}

AstNode parse_expression(ref ParseCtx ctx, Precedence precedence = Precedence.Assign) {
    auto expression = prefix_parselets[ctx.tokens.current.type](ctx);

    while (precedence <= infix_parselets[ctx.tokens.current.type].precedence) {
        if (ctx.tokens.current.type != Token.Rarrow)
            expression = try_unwrap_list(ctx, expression);

        expression = infix_parselets[ctx.tokens.current.type].parselet(ctx, expression);
    }
    expression = try_unwrap_list(ctx, expression);

    return expression;
}

AstNode parse_type(ref ParseCtx ctx, Precedence precedence = Precedence.Call) {
    auto expression = get_type_prefix_parselet(ctx.tokens.current.type)(ctx);

    while (precedence <= get_type_infix_parselet(ctx.tokens.current.type).precedence)
        expression = get_type_infix_parselet(ctx.tokens.current.type).parselet(ctx, expression);

    return expression;
}

private:

// ----------------------------------------------------------------------
//    _____  _          _                                 _        
//   / ____|| |        | |                               | |       
//  | (___  | |_  __ _ | |_  ___  _ __ ___    ___  _ __  | |_  ___ 
//   \___ \ | __|/ _` || __|/ _ \| '_ ` _ \  / _ \| '_ \ | __|/ __|
//   ____) || |_| (_| || |_|  __/| | | | | ||  __/| | | || |_ \__ \
//  |_____/  \__|\__,_| \__|\___||_| |_| |_| \___||_| |_| \__||___/
//
// ----------------------------------------------------------------------

AstNode parse_def(ref ParseCtx ctx) {
    const start_span = ctx.tokens.take().span;

    auto name = parse_key_type!(AstNode.Name)(ctx);

    ctx.tokens.skip_required(Token.Colon, ctx);

    auto type = ctx.tokens.current.type != Token.Equals ?
                parse_type(ctx) :
                AstNode.inferred_type;

    auto value = ctx.tokens.skip(Token.Equals) ?
                 parse_expression(ctx) :
                 AstNode.none;

    const span = start_span.merge(ctx.tokens.take_required(Token.Semicolon, ctx).span);
    return new AstNode(AstNode.Define, span, ctx.node_array(name, type, value));
}

AstNode parse_var(ref ParseCtx ctx, AstNode lhs) {
    ctx.tokens.advance();

    assert(lhs.type == AstNode.Name);

    auto type = ctx.tokens.current.type != Token.Equals ?
                parse_type(ctx) :
                AstNode.inferred_type;

    auto value = ctx.tokens.skip(Token.Equals) ?
                 parse_expression(ctx) : 
                 AstNode.none;

    const span = lhs.span.merge(type.span);
    return new AstNode(AstNode.Variable, span, ctx.node_array(lhs, type, value));
}

AstNode parse_loop(ref ParseCtx ctx) {
    auto start_span = ctx.tokens.take().span;

    auto body = parse_statement(ctx);

    return new AstNode(AstNode.Loop, start_span.merge(body.span), body);
}

AstNode parse_if(ref ParseCtx ctx) {
    const start_span = ctx.tokens.take().span;

    auto condition = parse_expression(ctx);

    auto body = parse_statement(ctx);
    
    auto else_branch = ctx.tokens.skip(Token.Else) ?
                       parse_statement(ctx) :
                       AstNode.none;
    
    const span = start_span.merge(body.span).merge(else_branch.span);
    return new AstNode(AstNode.If, span, ctx.node_array(condition, body, else_branch));
}

AstNode parse_escape(AstNode.Type Type, Token.Type ttype, bool with_value)(ref ParseCtx ctx) {
    auto start_span = ctx.tokens.take().span;

    static if (with_value) {
        auto value = prefix_parselets[ctx.tokens.current.type] != prefix_parselets[Token.Invalid] ?
                     parse_expression(ctx) :
                     AstNode.none;

        const span = start_span.merge(ctx.tokens.take_required(Token.Semicolon, ctx).span);
        return new AstNode(Type, span, value);
    }
    else {
        ctx.tokens.skip_required(Token.Semicolon, ctx);
        return new AstNode(Type, start_span);
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

alias PrefixParseFn = AstNode function(ref ParseCtx ctx);
alias InfixParseFn = AstNode function(ref ParseCtx ctx, AstNode lhs);

struct Infix { Precedence precedence; InfixParseFn parselet; }

immutable prefix_parselets = () {
    PrefixParseFn[256] parselets = (ref ctx) {
        error(
            ctx,
            SyntaxError.TokenNotAnExpression,
            "The token \"%s\" cannot start an expression.",
            ctx.tokens.current.type
        );

        return new AstNode(AstNode.Invalid, ctx.tokens.current.span);
    };

    parselets[Token.Done]       = (ref ctx) {
        error(
            ctx,
            SyntaxError.UnexpectedEndOfFile,
            "Unexpected end of file while parsing source."
        );

        return new AstNode(AstNode.Invalid, ctx.tokens.current.span);
    };

    parselets[Token.Name]       = &parse_key_type!(AstNode.Name);
    parselets[Token.Integer]    = &parse_key_type!(AstNode.Integer);
    parselets[Token.Char]       = &parse_key_type!(AstNode.Char);
    parselets[Token.Minus]      = &parse_unary!(AstNode.Negate);
    parselets[Token.Bang]       = &parse_unary!(AstNode.Falsify);
    parselets[Token.Ampersand]  = &parse_unary!(AstNode.GetRef);
    parselets[Token.Star]       = &parse_unary!(AstNode.Pointer);
    parselets[Token.Lparen]     = &parse_seq!(AstNode.List, parse_list_member, Token.Rparen);
    parselets[Token.Lbracket]   = &parse_seq!(AstNode.List, parse_list_member, Token.Rbracket);
    parselets[Token.Lbrace]     = &parse_block;

    return parselets;
} ();

immutable infix_parselets = () {
    Infix[256] parselets;

    static set(AstNode.Type type, Token.Type ttype, Precedence prec, bool left_assoc = true, bool skip_op = true)(ref Infix[256] parselets) {
        parselets[ttype] = Infix(prec, &parse_binary!(type, left_assoc ? prec + 1 : prec, skip_op));
    }

    set!(AstNode.Assign,        Token.Equals,       Precedence.Assign   )(parselets);
    set!(AstNode.Less,          Token.Less,         Precedence.Compare  )(parselets);
    set!(AstNode.LessEqual,     Token.LessEqual,    Precedence.Compare  )(parselets);
    set!(AstNode.Greater,       Token.Greater,      Precedence.Compare  )(parselets);
    set!(AstNode.GreaterEqual,  Token.GreaterEqual, Precedence.Compare  )(parselets);
    set!(AstNode.Equal,         Token.EqualEqual,   Precedence.Equality )(parselets);
    set!(AstNode.NotEqual,      Token.BangEqual,    Precedence.Equality )(parselets);
    set!(AstNode.And,           Token.And,          Precedence.Logic    )(parselets);
    set!(AstNode.Or,            Token.Or,           Precedence.Logic    )(parselets);
    set!(AstNode.Add,           Token.Plus,         Precedence.Sum      )(parselets);
    set!(AstNode.Subtract,      Token.Minus,        Precedence.Sum      )(parselets);
    set!(AstNode.Multiply,      Token.Star,         Precedence.Product  )(parselets);
    set!(AstNode.Divide,        Token.Slash,        Precedence.Product  )(parselets);
    set!(AstNode.Power,         Token.Caret,        Precedence.Power,   false)(parselets);
    set!(AstNode.Call,          Token.Dot,          Precedence.Call     )(parselets);
    set!(AstNode.Call,          Token.Lparen,       Precedence.Call,    true, false)(parselets);
    set!(AstNode.Call,          Token.Lbracket,     Precedence.Call,    true, false)(parselets);
    parselets[Token.Colon]  = Infix(Precedence.Assign, &parse_var);
    parselets[Token.Rarrow] = Infix(Precedence.Primary, &parse_function);

    return parselets;
} ();

auto parse_key_type(AstNode.Type type)(ref ParseCtx ctx) {
    auto t = ctx.tokens.take();
    return new AstNode(type, t.span, t.key);
}

auto parse_unary(AstNode.Type type)(ref ParseCtx ctx) {
    auto op_span = ctx.tokens.take().span;
    auto operand = parse_expression(ctx);

    return new AstNode(type, op_span.merge(operand.span), operand);
}

AstNode parse_binary(AstNode.Type type, int precedence, bool skip_operator)(ref ParseCtx ctx, AstNode lhs) {
    static if (skip_operator)
        ctx.tokens.advance();
    
    auto rhs = parse_expression(ctx, cast(Precedence) precedence);

    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid) {
        return new AstNode(type, lhs.span.merge(rhs.span), ctx.node_array(lhs, rhs));
    }
    else {
        scope(exit) ctx.free(lhs, rhs);
        return new AstNode(AstNode.Invalid, lhs.span.merge(rhs.span));
    }
}

AstNode parse_seq(AstNode.Type type, alias parse_member, Token.Type close_delim)(ref ParseCtx ctx) {
    ctx.tokens.push_eol_delimiter(Token.Comma);
    const open_span = ctx.tokens.take().span;

    ctx.tokens.skip(Token.Comma);

    AstNode[] members;
    while (!ctx.tokens.current.type.matches_one(close_delim, Token.Done)) {
        members ~= parse_member(ctx);

        ctx.tokens.skip(Token.Comma);
    }

    ctx.tokens.pop_eol_delimiter();
    const close_token = ctx.tokens.take();

    if (close_token.type == close_delim) {
        return new AstNode(type, open_span.merge(close_token.span), members);
    }
    else {
        assert(close_token.type == Token.Done);
        
        ctx.free(members);
        
        bool has_eof_error;
        foreach (error; ctx.errors)
            if (error.code == SyntaxError.UnexpectedEndOfFile) {
                has_eof_error = true;
                break;
            }

        if (!has_eof_error)
            error(
                ctx,
                SyntaxError.UnexpectedEndOfFile,
                "Unexpected end of file while parsing source."
            );

        return new AstNode(AstNode.Invalid, open_span.merge(close_token.span));
    }
}

AstNode parse_list_member(ref ParseCtx ctx) {
    auto first = parse_expression(ctx, Precedence.Logic);

    const is_name = first.type == AstNode.Name;
    auto name = is_name && ctx.tokens.current.type.matches_one(Token.Equals, Token.Colon) ?
                first :
                AstNode.none;
    
    auto type = ctx.tokens.skip(Token.Colon) ?
                parse_type(ctx) :
                AstNode.inferred_type;
    
    auto expr = ctx.tokens.skip(Token.Equals) ?
                parse_expression(ctx) :
                name.type == AstNode.None ?
                    first :
                    AstNode.none;

    const span = name.span.merge(type.span).merge(expr.span);
    if (name.type != AstNode.Invalid && type.type != AstNode.Invalid && expr.type != AstNode.Invalid) {
        return new AstNode(AstNode.ListMember, span, ctx.node_array(name, type, expr));
    }
    else {
        ctx.free(name, type, expr);
        return new AstNode(AstNode.Invalid, span);
    }
}

AstNode parse_block(ref ParseCtx ctx) {
    ctx.tokens.push_eol_delimiter(Token.Semicolon);
    const open_span = ctx.tokens.take().span;

    AstNode[] statements;
    while (!ctx.tokens.current.type.matches_one(Token.Done, Token.Rbrace)) {
        statements ~= parse_statement(ctx);
    }

    ctx.tokens.pop_eol_delimiter();
    const close_token = ctx.tokens.take();

    if (close_token.type == Token.Rbrace) {
        return new AstNode(AstNode.Block, open_span.merge(close_token.span), statements);
    }
    else {
        assert(close_token.type == Token.Done);

        error(
            ctx,
            SyntaxError.UnexpectedEndOfFile,
            "Unexpected end of file while parsing source."
        );

        // it's the end of the file, raise an error, but close the block so we can do further processing
        return new AstNode(AstNode.Block, open_span.merge(close_token.span), statements);
    }
}

AstNode parse_function(ref ParseCtx ctx, AstNode params) {
    ctx.tokens.skip_required(Token.Rarrow, ctx);

    // expression or type ... depends on what comes first, a semicolon, or a block
    // Copy the lexer so that we can do lookahead.
    // Yes, this does involve copying the delimiter array, but it's probably not
    // going to be very large, so we can get away with it for now.
    auto lookahead = ctx.tokens;
    while (!lookahead.current.type.matches_one(Token.Done, Token.Comma, Token.Semicolon, Token.Lbrace))
        lookahead.advance();
    
    auto ret_type = lookahead.current.type == Token.Lbrace && lookahead.current != ctx.tokens.current ?
                    parse_type(ctx) :
                    AstNode.inferred_type;
    
    auto body = parse_expression(ctx) ;

    const span = params.span.merge(ret_type.span).merge(body.span);
    if (params.type != AstNode.Invalid && ret_type.type != AstNode.Invalid && body.type != AstNode.Invalid)
        return new AstNode(AstNode.Function, span, ctx.node_array(params, ret_type, body));
    else {
        ctx.free(params, ret_type, body);
        return new AstNode(AstNode.Invalid, span);
    }
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

PrefixParseFn get_type_prefix_parselet(Token.Type t) {
    switch (t) {
        case Token.Name:
            return &parse_key_type!(AstNode.Name);

        case Token.Star:
            return &parse_unary!(AstNode.PointerType);

        case Token.Lparen:
            return &parse_seq!(AstNode.TypeList, parse_type_list_member, Token.Rparen);

        case Token.Lbracket:
            return &parse_seq!(AstNode.TypeList, parse_type_list_member, Token.Rbracket);

        default:
            return (ref ctx) {
                error(
                    ctx,
                    SyntaxError.TokenNotAnExpression,
                    "The token \"%s\" cannot start an expression.",
                    ctx.tokens.current.type
                );
                return new AstNode(AstNode.Invalid, ctx.tokens.current.span);
            };
    }
}

Infix get_type_infix_parselet(Token.Type t) {
    switch (t) {
        case Token.Dot:
            return Infix(Precedence.Call, &parse_binary!(AstNode.Call, Precedence.Primary, true));

        case Token.Lparen:
            return Infix(Precedence.Call, &parse_binary!(AstNode.Call, Precedence.Primary, false));

        case Token.Lbracket:
            return Infix(Precedence.Call, &parse_binary!(AstNode.Call, Precedence.Primary, false));

        case Token.Rarrow:
            return Infix(Precedence.Primary, &parse_function_type);
        default:
            return Infix();
    }
}

AstNode parse_type_list_member(ref ParseCtx ctx) {
    auto first = parse_type(ctx);

    auto name = first.type == AstNode.Name && ctx.tokens.current.type == Token.Colon ?
                first :
                AstNode.none;
    
    auto type = ctx.tokens.skip(Token.Colon) ?
                parse_type(ctx) :
                first ;

    const span = name.span.merge(type.span);
    if (name.type != AstNode.Invalid && type.type != AstNode.Invalid) {
        return new AstNode(AstNode.TypeListMember, span, ctx.node_array(name, type));
    }
    else {
        ctx.free(name, type);
        return new AstNode(AstNode.Invalid, span);
    }
}

AstNode parse_function_type(ref ParseCtx ctx, AstNode lhs) {
    assert(lhs.type == AstNode.TypeList || lhs.type == AstNode.Invalid);

    ctx.tokens.skip_required(Token.Rarrow, ctx);

    auto return_type = parse_type(ctx);
    const span = lhs.span.merge(return_type.span);

    if (lhs.type != AstNode.Invalid && return_type.type != AstNode.Invalid) {
        return new AstNode(AstNode.FunctionType, span, ctx.node_array(lhs, return_type));
    }
    else {
        ctx.free(lhs, return_type);
        return new AstNode(AstNode.Invalid, span);
    }
}

// ----------------------------------------------------------------------
//   _    _  _    _  _  _  _    _            
//  | |  | || |  (_)| |(_)| |  (_)           
//  | |  | || |_  _ | | _ | |_  _   ___  ___ 
//  | |  | || __|| || || || __|| | / _ \/ __|
//  | |__| || |_ | || || || |_ | ||  __/\__ \
//   \____/  \__||_||_||_| \__||_| \___||___/
// ----------------------------------------------------------------------

Token take(ref Lexer l) {
    scope(exit) l.advance();
    return l.current;
}

bool skip(ref Lexer l, Token.Type t) {
    if (l.current.type != t)
        return false;
    l.advance();
    return true;
}

Token take_required(ref Lexer l, Token.Type t, ref ParseCtx ctx) {
    auto token = l.take();
    if (token.type == t)
        return token;
    
    error(
        ctx,
        SyntaxError.TokenExpectMismatch,
        "The token %s does not match the expected token type %s",
        l.span.get_text(l.current.span),
        t
    );

    assert(false, "Error: Expect mismatch!");
}

void skip_required(ref Lexer l, Token.Type t, ref ParseCtx ctx) {
    if (l.skip(t))
        return;
    
    error(
        ctx,
        SyntaxError.TokenExpectMismatch,
        "The token %s does not match the expected token type %s",
        l.span.get_text(l.current.span),
        t
    );

    assert(false, "Error: Expect mismatch!");
}

void error(Args...)(ref ParseCtx ctx, SyntaxError.Code error_code, string message, Args args) {
    ctx.errors ~= SyntaxError(
        error_code,
        ctx.source_text,
        ctx.tokens.current.span.start,
        tprint(message, args).idup
    );
}

const(char[]) tprint(Args...)(string message, Args args) {
    import std.format: formattedWrite;

    static struct Buffer {
        char[] data;
        size_t length;

        void put(char c) {
            assert(length < data.length);
            data[length] = c;
            length++;
        }

        const(char[]) text() const { return data[0 .. length]; }
    }

    static char[4096] temp_buffer;

    auto buffer = Buffer(temp_buffer);
    formattedWrite(buffer, message, args);
    
    return buffer.text();
}

/**
 * Attempts to unwrap an AstNode as a single-element list.
 * If the node is not a single-element list, or if the list
 * member is invalid, this function returns the node
 * untouched.
 */
AstNode try_unwrap_list(ref ParseCtx ctx, AstNode node) {
    const can_try_unwrap = node.type == AstNode.List && node.get_children().length == 1;

    if (!can_try_unwrap)
        return node;

    auto member_parts = node.get_children()[0].get_children();
    
    // the member is invalid or is not just a value
    if (member_parts.length == 0 || member_parts[0].type != AstNode.None)
        return node;
    
    assert(member_parts[1].type == AstNode.InferredType);

    auto value = member_parts[2];
    member_parts[2] = AstNode.none;
    ctx.free(node);

    return value;
}
