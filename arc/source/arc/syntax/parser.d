module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.location: SpannedText;

struct Parser {
    import arc.syntax.syntax_reporter: SyntaxReporter;
    import arc.stringtable: StringTable;
    import arc.syntax.location: SpannedText;

    ///
    Lexer lexer;
    ///
    StringTable *table;
    ///
    SyntaxReporter *error;

    alias lexer this;

    ///
    this(SpannedText source, StringTable* table, SyntaxReporter* error) {
        lexer = Lexer(source, table);
        lexer.ready();
        this.error = error;
    }

    ///
    bool consume(Token.Type type) {
        if (type != current.type)
            return false;
        
        advance();
        return true;
    }

    bool empty() { return current.type == Token.Eof; }
}

AstNode statement(ref Parser p) {
    AstNode s;
    p.push_eol_type(Token.Semicolon);
    switch (p.current.type) with (Token.Type) {
        case Def:
            s = def(p);
            break;
        default:
            s = expression(p);
            break;
    }
    p.consume(Token.Semicolon);
    p.pop_eol_type();
    return s;
}

bool is_primary_token(Token.Type t) {
    switch (t) with (Token.Type) {
    case Name:
    case Char:
    // case Minus:
    case Lparen:    // our good fortune that () and a() are both primaries
    case Lbracket:  // ditto
    case Dot:
    case FatRArrow:
        return true;
    default:
        return false;
    }
}

bool is_expr_token(Token.Type t) {
    return (prefix_parslets[t] !is &null_prefix) || (infix_parslets[t].parselet !is null);
}

AstNode primary(ref Parser p, Infix.Precedence prec) {
    auto node = p.prefix();
    while (is_primary_token(p.current.type) && prec <= infix_parslets[p.current.type].precedence) {
        node = infix_parslets[p.current.type].parselet(p, node);
    }
    return node;
}

alias Prefix = AstNode function(ref Parser);

/// List of functions for parsing tokens that can start expressions
immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p = &null_prefix;

    p[Token.Lparen] = &paren_list;
    p[Token.Lbracket] = &bracket_list;
    p[Token.Lbrace] = &block;
    p[Token.Name] = &name;
    p[Token.Integer] = &integer;
    p[Token.Char] = &char_;
    p[Token.Minus] = &negate;
    p[Token.Dot] = &unary_dot;

    return p;
} ();

AstNode null_prefix(ref Parser p) {
    p.error.token_cannot_start_expr(p.current);
    scope(exit) p.advance();
    return make_invalid(p.current.span);
}

AstNode prefix(ref Parser p) {
    auto parselet = prefix_parslets[p.current.type];
    return parselet(p);
}

/// Name := ('_' | $a-zA-Z)* ;
AstNode name(ref Parser p) {
    scope(exit) p.advance();
    return make_name(p.current.span, p.current.key);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
AstNode integer(ref Parser p) {
    import std.conv: to;

    scope(exit) p.advance();
    return make_int(p.current.span, p.current.span.text.to!ulong);
}

AstNode char_(ref Parser p) {
    scope(exit) p.advance();
    return make_char(p.current.span);
}

alias paren_list = list!(AstNode.List, Token.Lparen, Token.Rparen);
alias bracket_list = list!(AstNode.List, Token.Lbracket, Token.Rbracket);

AstNode list(AstNode.Type t, Token.Type open, Token.Type close)(ref Parser p) {
    auto span = p.current.span.span;
    p.push_eol_type(Token.Comma);
    p.consume(open);

    while(p.consume(Token.Comma)) continue;
    AstNode[] members;
    if (p.current.type != close) do {
        auto e = p.var_expr();
        span = span.merge(e.span); // for error reporting
        members ~= e;

        while(p.consume(Token.Comma))
            continue;

        if (!is_expr_token(p.current.type) && (p.current.type != close)) {
            scope(exit) p.advance();
            p.pop_eol_type();
            p.error.list_not_closed(span, p.current.span);
            return make_invalid(span.merge(p.current.span));
        }
    } while (p.current.type != close);

    span = span.merge(p.current.span);
    p.pop_eol_type();
    p.advance();
    return make_n_ary(t, span, members);
}

AstNode block(ref Parser p) {
    auto span = p.current.span.span;
    p.consume(Token.Lbrace);
    p.push_eol_type(Token.Semicolon);

    AstNode[] members;
    while (p.current.type != Token.Rbrace) {
        if (p.current.type == Token.Eof) {
            p.error.unexpected_end_of_file(span);
        }

        auto s = p.statement();
        span = span.merge(s.span);
        members ~= s;
    }

    p.pop_eol_type();
    span = span.merge(p.current.span);
    p.consume(Token.Rbrace);
    return make_n_ary(AstNode.Block, span, members);
}

/// Function := Expression '=>' Expression ;
AstNode function_(ref Parser p, AstNode params) {
    p.consume(Token.FatRArrow);
    auto body = p.expression();
    return make_binary(AstNode.Function, params, body);
}

/// Unary := <op> Expression
AstNode unary(AstNode.Type t, Token.Type ttype)(ref Parser p) {
    const start = p.current;
    p.consume(ttype);
    auto expr = p.expression();
    return make_n_ary(t, start.span.merge(expr.span), expr);
}

alias negate = unary!(AstNode.Negate, Token.Minus);
alias unary_dot = unary!(AstNode.SelfCall, Token.Dot);

/// Convenience struct to hold the precedence and parser for an infix expression
struct Infix {
    enum Precedence {
        None,
        Assignment,
        Equality,
        Comparison,
        Sum,
        Product,
        Power,
        Call,
        Primary,
    }

    Precedence precedence;

    alias InfixParselet = AstNode function(ref Parser, AstNode);
    InfixParselet parselet;
    
    alias Precedence this;
}

/// List of functions for parsing infix expressions (binary operators, calls, etc)
immutable Infix[256] infix_parslets = () {
    Infix[256] p;

    p[Token.FatRArrow]  = Infix(Infix.Primary, &function_);
    p[Token.Plus]       = Infix(Infix.Sum, &add);
    p[Token.Minus]      = Infix(Infix.Sum, &subtract);
    p[Token.Star]       = Infix(Infix.Product, &multiply);
    p[Token.Slash]      = Infix(Infix.Product, &divide);
    p[Token.Caret]      = Infix(Infix.Power, &power);
    // p[Token.Equals]     = Infix(Infix.Assignment, &assign);
    p[Token.Lparen]     = Infix(Infix.Call, &call);
    p[Token.Lbracket]   = Infix(Infix.Call, &call);
    p[Token.Dot]        = Infix(Infix.Call, &dot);
    p[Token.Colon]      = Infix(Infix.Primary, &var_expr2);

    return p;
} ();


AstNode expression(ref Parser p, Infix.Precedence prec = Infix.Precedence.Assignment) {
    auto expr = p.prefix();
    if (expr.type != AstNode.Invalid) {
        while (prec <= infix_parslets[p.current.type].precedence)
            expr = infix_parslets[p.current.type].parselet(p, expr);
    }

    return expr;
}

/// Binary := Expression <op> Expression ;
AstNode binary(AstNode.Type t, int prec, Token.Type ttype)(ref Parser p, AstNode lhs) {
    p.consume(ttype);
    auto rhs = p.expression(cast(Infix.Precedence) prec);
    return make_binary(t, lhs, rhs);
}

alias add = binary!(AstNode.Add, Infix.Sum + 1, Token.Plus);
alias subtract = binary!(AstNode.Subtract, Infix.Sum + 1, Token.Minus);
alias multiply = binary!(AstNode.Multiply, Infix.Product + 1, Token.Star);
alias divide = binary!(AstNode.Divide, Infix.Product + 1, Token.Slash);
// not Infix.Power + 1 because we want this to be right-associative
alias power = binary!(AstNode.Power, Infix.Power, Token.Caret);
alias dot = binary!(AstNode.Call, Infix.Call + 1, Token.Dot);

AstNode dot(ref Parser p, AstNode lhs) {
    assert(lhs.type == AstNode.Name || lhs.type == AstNode.List || lhs.type == AstNode.Call);
    p.consume(Token.Dot);

    auto node = lhs;
    while (p.current.type == Token.Name)
        node = make_binary(AstNode.Call, node, prefix_parslets[p.current.type](p));

    return node;
}

/// Call := Expression List
AstNode call(ref Parser p, AstNode lhs) {
    auto node = lhs;
    while (p.current.type == Token.Lparen || p.current.type == Token.Lbracket) {
        node = make_binary(AstNode.Call, node, prefix_parslets[p.current.type](p));
    }
    return node;
}

/**
 * var_expr : Expression ((":" (('=' Expression) | (Expression ("=" Expression)?)) |
 *                        ("=" Expression))?
 */
AstNode var_expr(ref Parser p) {
    return var_expr2(p, p.primary(Infix.Call));
}

AstNode var_expr2(ref Parser p, AstNode first) {
    AstNode name_expr, type_expr, value_expr;
    auto span = first.span;

    if (p.consume(Token.Colon)) {
        name_expr = first;
        if (p.consume(Token.Equals)) {
            value_expr = p.primary(Infix.Call);
            type_expr = AstNode.none;
            span = span.merge(value_expr.span);
        }
        else {
            // var_expr : Expression (":" Expression)?
            type_expr = p.primary(Infix.Call);
            span = span.merge(type_expr.span);

            if (p.consume(Token.Equals)) {
                // var_expr : Expression (":" Expression ("=" Expression)?)?
                value_expr = p.expression();
                span = span.merge(value_expr.span);
            }
            else {
                value_expr = AstNode.none;
            }
        }
    }
    else if (p.consume(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        name_expr = first;
        value_expr = p.expression();
        type_expr = AstNode.none;
        span = span.merge(value_expr.span);
    }
    else {
        // var_expr : Expression
        value_expr = first;
        name_expr = AstNode.none;
        type_expr = AstNode.none;
    }

    return make_n_ary(AstNode.VarExpression, span, name_expr, type_expr, value_expr);
}

AstNode def(ref Parser p) {
    auto tok = p.current;
    p.consume(Token.Def);
    auto expr = p.var_expr();
    return make_n_ary(AstNode.Define, tok.span.merge(expr.span), expr);
}
