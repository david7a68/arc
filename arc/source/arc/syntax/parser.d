module arc.syntax.parser;

import arc.hash: Key;
import arc.syntax.ast;
import arc.syntax.lexer: Lexer, Token;
import arc.syntax.location: Span;
import arc.syntax.reporter: SyntaxReporter, SyntaxError;

enum ExprPrecedence {
    None,
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
    SyntaxReporter errors;

    this(Lexer lexer, SyntaxReporter reporter) {
        tokens = lexer;
        errors = reporter;
    }

    void free(AstNode[] nodes...) {
        foreach (node; nodes) if (!is_marker(node.type)) destroy(node);
    }
}

Module parse_module(ref ParseCtx ctx) {
    ctx.tokens.push_eol_delimiter(Token.Semicolon);

    // Cleanup: This should be handled by ctx.nodes, not by accessing the GC directly
    AstNode[] statements;
    while (!ctx.tokens.empty)
        statements ~= parse_statement(ctx);

    ctx.tokens.pop_eol_delimiter();
    return new Module(ctx.tokens.span, statements);
}

AstNode parse_statement(ref ParseCtx ctx) {
    switch (ctx.tokens.current.type) {
        case Token.Def:
            return parse_def(ctx);
        case Token.Break:
            return parse_control_flow!(Break, Token.Break, false)(ctx);
        case Token.Return:
            return parse_control_flow!(Return, Token.Return, true)(ctx);
        case Token.Continue:
            return parse_control_flow!(Continue, Token.Continue, false)(ctx);
        case Token.If:
            return parse_if(ctx);
        case Token.Loop:
            return parse_loop(ctx);
        default:
            scope(exit)
                ctx.tokens.skip_required(Token.Semicolon, &ctx.errors);
            
            auto first = parse_expression(ctx, ExprPrecedence.Call);
            // a : b ... c = d
            if (first.type == AstNode.Name) {
                // assert(false);
                if (ctx.tokens.current.type == Token.Colon)
                    return parse_var(ctx, first);
                else if (ctx.tokens.current.type == Token.Equals)
                    return parse_binary!(Assign, ExprPrecedence.Logic, true)(ctx, first);
            }
            // a.b().c = 3
            else if (first.type == AstNode.Call && ctx.tokens.current.type == Token.Equals) {
                return parse_binary!(Assign, ExprPrecedence.Logic, true)(ctx, first);
            }
            return parse_infix(ctx, first);
    }
}

AstNode parse_expression(ref ParseCtx ctx, ExprPrecedence precedence = ExprPrecedence.Logic) {
    const prefix_parselet = prefix_parselets[ctx.tokens.current.type];
    auto expression = prefix_parselet(ctx);

    return parse_infix(ctx, expression, precedence);
}

AstNode parse_type(ref ParseCtx ctx, ExprPrecedence precedence = ExprPrecedence.Call) {
    const prefix_parselet = get_type_prefix_parselet(ctx.tokens.current.type);
    auto expression = prefix_parselet(ctx);

    while (precedence <= get_type_infix_parselet(ctx.tokens.current.type).precedence)
        expression = get_type_infix_parselet(ctx.tokens.current.type).parselet(ctx, expression);

    return expression;
}

private:

Token take_required(ref Lexer l, Token.Type t, SyntaxReporter* reporter) {
    auto token = l.take();
    if (token.type == t)
        return token;
    
    reporter.error(
        SyntaxError.TokenExpectMismatch,
        l.current.span.start,
        "The token %s does not match the expected token type %s",
        l.span.get_text(l.current.span),
        t
    );
    assert(false, "Error: Expect mismatch!");
}

void skip_required(ref Lexer l, Token.Type t, SyntaxReporter* reporter) {
    if (l.skip(t))
        return;
    
    reporter.error(
        SyntaxError.TokenExpectMismatch,
        l.current.span.start,
        "The token %s does not match the expected token type %s",
        l.span.get_text(l.current.span),
        t
    );
    assert(false, "Error: Expect mismatch!");
}

alias PrefixParselet = AstNode function(ref ParseCtx ctx);
alias InfixParselet = AstNode function(ref ParseCtx ctx, AstNode lhs);

struct Infix { ExprPrecedence precedence; InfixParselet parselet; }

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

    auto name = parse_key_type!(Name)(ctx);

    ctx.tokens.skip_required(Token.Colon, &ctx.errors);

    auto type = ctx.tokens.current.type != Token.Equals ?
                parse_type(ctx) :
                TypeExpression.inferred;

    auto value = ctx.tokens.skip(Token.Equals) ?
                 parse_expression(ctx) :
                 AstNode.none;

    const span = start_span.merge(ctx.tokens.take_required(Token.Semicolon, &ctx.errors).span);
    return new Define(span, name, type, value);
}

AstNode parse_var(ref ParseCtx ctx, AstNode lhs) {
    ctx.tokens.drop();

    assert(lhs.type == AstNode.Name);

    auto type = ctx.tokens.current.type != Token.Equals ?
                parse_type(ctx) :
                TypeExpression.inferred;

    auto value = ctx.tokens.skip(Token.Equals) ?
                 parse_expression(ctx) : 
                 AstNode.none;

    const span = lhs.span.merge(type.span);
    return new Variable(span, lhs, type, value);
}

AstNode parse_loop(ref ParseCtx ctx) {
    auto start_span = ctx.tokens.take().span;
    auto body = parse_statement(ctx);

    return new Loop(start_span.merge(body.span), body);
}

AstNode parse_if(ref ParseCtx ctx) {
    assert(ctx.tokens.eol_stack.back == Token.Semicolon);

    static parse_body(ref ParseCtx ctx) {
        return parse_statement(ctx);
        // return ctx.tokens.current.type == Token.Lbrace ? parse_block(ctx) : parse_statement(ctx);
    }

    const start_span = ctx.tokens.take().span;

    auto condition = parse_expression(ctx);
    auto body = parse_body(ctx);
    
    auto else_branch = ctx.tokens.skip(Token.Else) ?
                       parse_body(ctx) :
                       AstNode.none;
    
    const span = start_span.merge(body.span).merge(else_branch.span);
    return new If(span, condition, body, else_branch);
}

AstNode parse_control_flow(NodeType, Token.Type ttype, bool with_value)(ref ParseCtx ctx) {
    auto start_span = ctx.tokens.take().span;

    static if (with_value) {
        auto value = prefix_parselets[ctx.tokens.current.type] != prefix_parselets[Token.Invalid] ?
                     parse_expression(ctx) :
                     AstNode.none;

        const span = start_span.merge(ctx.tokens.take_required(Token.Semicolon, &ctx.errors).span);
        return new NodeType(span, value);
    }
    else {
        ctx.tokens.skip_required(Token.Semicolon, &ctx.errors);
        return new NodeType(start_span);
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

PrefixParselet[] prefix_parselets = () {
    PrefixParselet[256] parselets = (ref ctx) {
        ctx.errors.error(
            SyntaxError.TokenNotAnExpression,
            ctx.tokens.current.span.start,
            "The token \"%s\" cannot start an expression.",
            ctx.tokens.current.type
        );

        return new Invalid(Span(ctx.tokens.current.span.start, 0));
    };

    parselets[Token.Done]       = (ref ctx) {
        ctx.errors.error(
            SyntaxError.UnexpectedEndOfFile,
            ctx.tokens.current.span.start,
            "Unexpected end of file while parsing source."
        );
        return new Invalid(ctx.tokens.current.span);
    };

    parselets[Token.Name]       = &parse_key_type!Name;
    parselets[Token.Integer]    = &parse_key_type!Integer;
    parselets[Token.Char]       = &parse_key_type!Char;
    parselets[Token.Minus]      = &parse_unary!Negate;
    parselets[Token.Bang]       = &parse_unary!Falsify;
    parselets[Token.Ampersand]  = &parse_unary!GetRef;
    parselets[Token.Star]       = &parse_unary!Pointer;
    parselets[Token.Lparen]     = &parse_seq!(List, parse_list_member, Token.Rparen);
    parselets[Token.Lbracket]   = &parse_seq!(List, parse_list_member, Token.Rbracket);
    parselets[Token.Lbrace]     = &parse_block;

    return parselets;
} ();

Infix[] infix_parselets = () {
    Infix[255] parselets;

    static set(Node, Token.Type ttype, ExprPrecedence prec, bool left_assoc = true, bool skip_op = true)(ref Infix[255] parselets) {
        parselets[ttype] = Infix(prec, &parse_binary!(Node, left_assoc ? prec + 1 : prec, skip_op));
    }

    set!(Less,          Token.Less,         ExprPrecedence.Compare  )(parselets);
    set!(LessEqual,     Token.LessEqual,    ExprPrecedence.Compare  )(parselets);
    set!(Greater,       Token.Greater,      ExprPrecedence.Compare  )(parselets);
    set!(GreaterEqual,  Token.GreaterEqual, ExprPrecedence.Compare  )(parselets);
    set!(Equal,         Token.EqualEqual,   ExprPrecedence.Equality )(parselets);
    set!(NotEqual,      Token.BangEqual,    ExprPrecedence.Equality )(parselets);
    set!(And,           Token.And,          ExprPrecedence.Logic    )(parselets);
    set!(Or,            Token.Or,           ExprPrecedence.Logic    )(parselets);
    set!(Add,           Token.Plus,         ExprPrecedence.Sum      )(parselets);
    set!(Subtract,      Token.Minus,        ExprPrecedence.Sum      )(parselets);
    set!(Multiply,      Token.Star,         ExprPrecedence.Product  )(parselets);
    set!(Divide,        Token.Slash,        ExprPrecedence.Product  )(parselets);
    set!(Power,         Token.Caret,        ExprPrecedence.Power,   false)(parselets);
    set!(Call,          Token.Dot,          ExprPrecedence.Call     )(parselets);
    set!(Call,          Token.Lparen,       ExprPrecedence.Call,    true, false)(parselets);
    set!(Call,          Token.Lbracket,     ExprPrecedence.Call,    true, false)(parselets);
    parselets[Token.Rarrow] = Infix(ExprPrecedence.Primary, &parse_function);

    return parselets;
} ();

AstNode parse_infix(ref ParseCtx ctx, AstNode prefix, ExprPrecedence precedence = ExprPrecedence.Logic) {
    auto expression = prefix;
    while (precedence <= infix_parselets[ctx.tokens.current.type].precedence) {
        expression = infix_parselets[ctx.tokens.current.type].parselet(ctx, expression);
    }

    return expression;
}

auto parse_key_type(NodeType)(ref ParseCtx ctx) {
    auto t = ctx.tokens.take();
    return new NodeType(t.span, t.key);
}

auto parse_unary(NodeType)(ref ParseCtx ctx) {
    auto op_span = ctx.tokens.take().span;
    auto operand = parse_expression(ctx);

    return new NodeType(op_span.merge(operand.span), operand);
}

AstNode parse_binary(NodeType, int precedence, bool skip_operator)(ref ParseCtx ctx, AstNode lhs) {
    static if (skip_operator) ctx.tokens.drop();
    auto rhs = parse_expression(ctx, cast(ExprPrecedence) precedence);

    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new NodeType(lhs.span.merge(rhs.span), lhs, rhs);
    else {
        scope(exit) ctx.free(lhs, rhs);
        return new Invalid(lhs.span.merge(rhs.span));
    }
}

AstNode parse_seq(NodeType, alias parse_member, Token.Type close_delim)(ref ParseCtx ctx) {
    ctx.tokens.push_eol_delimiter(Token.Comma);
    const open_span = ctx.tokens.take().span;

    ctx.tokens.skip(Token.Comma);

    AstNode[] members;
    while (!ctx.tokens.matches_one(close_delim, Token.Done)) {
        members ~= parse_member(ctx);

        ctx.tokens.skip(Token.Comma);
    }

    ctx.tokens.pop_eol_delimiter();
    const close_token = ctx.tokens.take();

    if (close_token.type == close_delim) {
        return new NodeType(open_span.merge(close_token.span), members);
    }
    else {
        assert(close_token.type == Token.Done);
        
        ctx.free(members);
        
        if (!ctx.errors.has_error(SyntaxError.UnexpectedEndOfFile))
            ctx.errors.error(
                SyntaxError.UnexpectedEndOfFile,
                close_token.span.start,
                "Unexpected end of file while parsing source."
            );

        return new Invalid(open_span.merge(close_token.span));
    }
}

AstNode parse_list_member(ref ParseCtx ctx) {
    auto first = parse_expression(ctx, ExprPrecedence.Logic);

    const is_name = first.type == AstNode.Name;
    AstNode name = is_name && ctx.tokens.matches_one(Token.Equals, Token.Colon) ?
                   first :
                   AstNode.none;
    
    AstNode type = ctx.tokens.skip(Token.Colon) ?
                   parse_type(ctx) :
                   TypeExpression.inferred;
    
    AstNode value = ctx.tokens.skip(Token.Equals) ?
                    parse_expression(ctx) :
                    name.type == AstNode.None ?
                        first :
                        AstNode.none;

    const span = name.span.merge(type.span).merge(value.span);
    if (name.type != AstNode.Invalid && type.type != AstNode.Invalid && value.type != AstNode.Invalid)
        return new List.Member(span, name, type, value);
    else {
        ctx.free(name, type, value);
        return new Invalid(span);
    }
}

AstNode parse_block(ref ParseCtx ctx) {
    ctx.tokens.push_eol_delimiter(Token.Semicolon);
    const open_span = ctx.tokens.take().span;

    AstNode[] statements;
    while (!ctx.tokens.matches_one(Token.Done, Token.Rbrace)) {
        statements ~= parse_statement(ctx);
    }

    ctx.tokens.pop_eol_delimiter();
    const close_token = ctx.tokens.take();

    if (close_token.type == Token.Rbrace)
        return new Block(open_span.merge(close_token.span), statements);
    else {
        assert(close_token.type == Token.Done);

        ctx.errors.error(
            SyntaxError.UnexpectedEndOfFile,
            ctx.tokens.current.span.start,
            "Unexpected end of file while parsing source."
        );

        // it's the end of the file, raise an error, but close the block so we can do further processing
        return new Block(open_span.merge(close_token.span), statements);
    }
}

AstNode parse_function(ref ParseCtx ctx, AstNode params) {
    ctx.tokens.skip_required(Token.Rarrow, &ctx.errors);

    // expression or type ... depends on what comes first, a semicolon, or a block
    // Copy the lexer so that we can do lookahead.
    // Yes, this does involve copying an array. A new method will be needed eventually.
    auto lookahead = ctx.tokens;
    while (!lookahead.matches_one(Token.Done, Token.Comma, Token.Semicolon, Token.Lbrace))
        lookahead.advance();
    
    const has_block = lookahead.current.type == Token.Lbrace;
    auto ret_type = has_block && lookahead.current != ctx.tokens.current ?
                    parse_type(ctx) :
                    TypeExpression.inferred;
    
    auto body = has_block ?
                parse_block(ctx) :
                parse_expression(ctx) ;

    const span = params.span.merge(ret_type.span).merge(body.span);
    if (params.type != AstNode.Invalid && ret_type.type != AstNode.Invalid && body.type != AstNode.Invalid)
        return new Function(span, params, ret_type, body);
    else {
        ctx.free(params, ret_type, body);
        return new Invalid(span);
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

PrefixParselet get_type_prefix_parselet(Token.Type t) {
    switch (t) {
        case Token.Name:
            return &parse_key_type!(Name);
        case Token.Star:
            return &parse_unary!(PointerType);
        case Token.Lparen:
            return &parse_seq!(TypeList, parse_type_list_member, Token.Rparen);
        case Token.Lbracket:
            return &parse_seq!(TypeList, parse_type_list_member, Token.Rbracket);
        default:
            return (ref ctx) {
                ctx.errors.error(
                    SyntaxError.TokenNotAnExpression,
                    ctx.tokens.current.span.start,
                    "The token \"%s\" cannot start an expression.",
                    ctx.tokens.current.type
                );
                return new Invalid(Span(ctx.tokens.current.span.start, 0));
            };
    }
}

Infix get_type_infix_parselet(Token.Type t) {
    switch (t) {
        case Token.Dot:
            return Infix(ExprPrecedence.Call, &parse_binary!(Call, ExprPrecedence.Primary, true));
        case Token.Lparen:
            return Infix(ExprPrecedence.Call, &parse_binary!(Call, ExprPrecedence.Primary, false));
        case Token.Lbracket:
            return Infix(ExprPrecedence.Call, &parse_binary!(Call, ExprPrecedence.Primary, false));
        case Token.Rarrow:
            return Infix(ExprPrecedence.Primary, &parse_function_type);
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
    
    if (name.type != AstNode.None) {
        assert(name.type == AstNode.Name);
        auto old = name;
        name = new Name(old.span, old.get_key);
        destroy(old);
    }

    const span = name.span.merge(type.span);
    if (name.type != AstNode.Invalid && type.type != AstNode.Invalid)
        return new TypeList.Member(span, name, type);
    else {
        ctx.free(name, type);
        return new Invalid(span);
    }
}

AstNode parse_function_type(ref ParseCtx ctx, AstNode lhs) {
    assert(lhs.type == AstNode.TypeList || lhs.type == AstNode.Invalid);

    ctx.tokens.skip_required(Token.Rarrow, &ctx.errors);

    auto return_type = parse_type(ctx);
    const span = lhs.span.merge(return_type.span);

    if (lhs.type != AstNode.Invalid && return_type.type != AstNode.Invalid)
        return new FunctionType(span, lhs, return_type);
    else {
        ctx.free(lhs, return_type);
        return new Invalid(span);
    }
}
