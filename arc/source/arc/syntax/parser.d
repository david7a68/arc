module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.location: SpannedText, Span;

struct Parser {
    import arc.syntax.syntax_reporter: SyntaxReporter;
    import arc.stringtable: StringTable;

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

debug bool matches(T)(T t, T[] types...) {
    foreach (type; types) {
        if (t == type) return true;
    }
    return false;
}


//-----------------------------------//
//            Statements             //
//-----------------------------------//


AstNode statement(ref Parser p) {
    AstNode s;
    p.push_eol_type(Token.Semicolon);
    switch (p.current.type) with (Token.Type) {
        case Def:
            s = def(p);
            break;
        case Break:
            s = break_(p);
            break;
        case Return:
            s = return_(p);
            break;
        case Continue:
            s = continue_(p);
            break;
        default:
            s = expression(p);
            break;
    }
    p.consume(Token.Semicolon);
    p.pop_eol_type();
    return s;
}

bool is_stat_token(Token.Type t) {
    switch (t) with (Token.Type) {
    case Name:
    case Lparen:
    case Lbracket:
    case Lbrace:
    case Dot:
	case If:
	case Loop:
    case Break:
    case Return:
    case Def:
    case Continue:
        return true;
    default:
        return false;
    }
}

/// def = 'def' name ':' primary? '=' expr ;
AstNode def(ref Parser p) {
    auto span = p.current.span;
    p.consume(Token.Def);
    
    auto name = p.name();
    while (p.current.type == Token.ColonColon)
        name = p.path(name);
    
    auto type = AstNode.none;
    if (!p.consume(Token.Colon)) {
        p.error.definition_missing_colon(span.merge(name.span), p.current.span);
        type = make_invalid(Span(0, 0));
    }
    else if (p.current.type != Token.Equals) {
        type = p.primary();
    }
    
    p.consume(Token.Equals);
    auto value = p.expression();

    return make_n_ary(AstNode.Define, span.merge(value.span), name, type, value);
}

AstNode break_(ref Parser p) {
    auto span = p.current.span;
    p.advance();

    AstNode label = AstNode.none;
    if (p.current.type == Token.Label) {
        label = make_label(p.current.span, p.current.key);
        p.advance();
    }

    AstNode value = AstNode.none;
    if (is_expr_token(p.current.type))
        value = expression(p);
    return make_n_ary(AstNode.Break, span, label, value);
}

AstNode return_(ref Parser p) {
    auto span = p.current.span;
    p.advance();

    auto label = AstNode.none;
    if (p.current.type == Token.Label) {
        label = make_label(p.current.span, p.current.key);
        p.advance();
    }

    auto value = AstNode.none;
    if (is_expr_token(p.current.type))
        value = expression(p);
    return make_n_ary(AstNode.Return, span, label, value);
}

AstNode continue_(ref Parser p) {
    auto span = p.current.span;
    p.advance();

    auto label = AstNode.none;
    if (p.current.type == Token.Label) {
        label = make_label(p.current.span, p.current.key);
        p.advance();
    }

    return make_n_ary(AstNode.Continue, span, label);
}


//-----------------------------------//
//            Expressions            //
//-----------------------------------//


alias Prefix = AstNode function(ref Parser);

/// List of functions for parsing tokens that can start expressions
immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p = &null_prefix;

    p[Token.Name] = &name;
    p[Token.Integer] = &integer;
    p[Token.Char] = &char_;
    p[Token.Lparen] = &paren_list;
    p[Token.Lbracket] = &bracket_list;
    p[Token.Lbrace] = &block;
    p[Token.Minus] = &negate;
    p[Token.Dot] = &self_call;
    p[Token.Label] = &label;
    p[Token.If] = &if_;
    p[Token.Loop] = &loop;
    p[Token.Ampersand] = &get_ref;
    p[Token.Star] = &pointer;

    return p;
} ();

AstNode null_prefix(ref Parser p) {
    p.error.token_cannot_start_expr(p.current);
    scope(exit) p.advance();
    return make_invalid(p.current.span);
}

/// Convenience struct to hold the precedence and parser for an infix expression
struct Infix {
    enum Precedence {
        None,
        Assignment,
        Logic,
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

    p[Token.FatRArrow]      = Infix(Infix.Primary, &function_);
    p[Token.Plus]           = Infix(Infix.Sum, &add);
    p[Token.Minus]          = Infix(Infix.Sum, &subtract);
    p[Token.Star]           = Infix(Infix.Product, &multiply);
    p[Token.Slash]          = Infix(Infix.Product, &divide);
    p[Token.Caret]          = Infix(Infix.Power, &power);
    p[Token.Less]           = Infix(Infix.Comparison, &less);
    p[Token.LessEqual]      = Infix(Infix.Comparison, &less_equal);
    p[Token.Greater]        = Infix(Infix.Comparison, &greater);
    p[Token.GreaterEqual]   = Infix(Infix.Comparison, &greater_equal);
    p[Token.EqualEqual]     = Infix(Infix.Equality, &equal);
    p[Token.BangEqual]      = Infix(Infix.Equality, &not_equal);
    p[Token.Equals]         = Infix(Infix.Assignment, &assign);
    p[Token.And]            = Infix(Infix.Logic, &and);
    p[Token.Or]             = Infix(Infix.Logic, &or);
    p[Token.Lparen]         = Infix(Infix.Call, &call);
    p[Token.Lbracket]       = Infix(Infix.Call, &call);
    p[Token.Dot]            = Infix(Infix.Call, &dot);
    p[Token.Colon]          = Infix(Infix.Primary, &var_expr2);
    p[Token.ColonColon]     = Infix(Infix.Primary, &path);

    return p;
} ();

bool is_expr_token(Token.Type t) {
    return (prefix_parslets[t] !is &null_prefix) || (infix_parslets[t].parselet !is null);
}

AstNode expression(ref Parser p, Infix.Precedence prec = Infix.Precedence.Assignment) {
    auto expr = prefix_parslets[p.current.type](p);

    if (expr.type != AstNode.Invalid) {
        while (prec <= infix_parslets[p.current.type].precedence)
            expr = infix_parslets[p.current.type].parselet(p, expr);
    }

    return expr;
}

AstNode label(ref Parser p) {
    auto token = p.current;
    p.advance();

    auto label = make_label(token.span, token.key);
    if (p.current.type == Token.If) {
        auto inner = if_(p);
        return make_n_ary(AstNode.Labeled, label.span.merge(inner.span), label, inner);
    }
    else if (p.current.type == Token.Loop) {
        auto inner = loop(p);
        return make_n_ary(AstNode.Labeled, label.span.merge(inner.span), label, inner);
    }
    else if (p.current.type == Token.Lbracket) {
        auto inner = block(p);
        return make_n_ary(AstNode.Labeled, label.span.merge(inner.span), label, inner);
    }
    else if (is_primary_token(p.current.type)) {
        auto inner = primary(p);
        assert(inner.type == AstNode.Function);
        return make_n_ary(AstNode.Labeled, label.span.merge(inner.span), label, inner);
    }
    else if (p.current.type == Token.Semicolon) {
        return label;
    }
    else {
        assert(false);
    }
}

bool is_primary_token(Token.Type t) {
    switch (t) with (Token.Type) {
    case Name:
    case Char:
    case Integer:
    case Lparen:    // our good fortune that () and a() are both primaries
    case Lbracket:  // ditto
    case Dot:
    case FatRArrow:
    case ColonColon:
    case Label:
    case Star:
    case Ampersand:
        return true;
    default:
        return false;
    }
}

AstNode primary(ref Parser p) {
    auto node = prefix_parslets[p.current.type](p);
    while (is_primary_token(p.current.type) && infix_parslets[p.current.type].parselet !is null) {
        node = infix_parslets[p.current.type].parselet(p, node);
    }
    return node;
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

alias paren_list = seq!(AstNode.List, '(', ')', ',', var_expr, is_expr_token);
alias bracket_list = seq!(AstNode.List, '[', ']', ',', var_expr, is_expr_token);
alias block = seq!(AstNode.Block, '{', '}', ';', statement, is_stat_token);

AstNode seq(AstNode.Type t, char open, char close, char separator, alias parse_element, alias type_fn)(ref Parser p) {
    auto span = p.current.span.span;
    p.push_eol_type(cast(Token.Type) separator);
    p.consume(cast(Token.Type) open);

    while (p.consume(cast(Token.Type) separator)) continue;
    
    AstNode[] members;
    if (p.current.type != cast(Token.Type) close) do {
        auto e = parse_element(p);
        span = span.merge(e.span);
        members ~= e;

        while (p.consume(cast(Token.Type) separator)) continue;

        if (!type_fn(p.current.type) && (p.current.type != close)) {
            scope(exit) p.advance();
            p.pop_eol_type();
            p.error.seq_not_closed(span, p.current.span, t);
            return make_invalid(span.merge(p.current.span));
        }
    } while (p.current.type != cast(Token.Type) close);

    span = span.merge(p.current.span);
    p.pop_eol_type();
    p.advance();
    return make_n_ary(t, span, members);
}

/**
 * var_expr : Expression ((":" (('=' Expression) | (Expression ("=" Expression)?)) |
 *                        ("=" Expression))?
 */
AstNode var_expr(ref Parser p) {
    return var_expr2(p, p.primary());
}

AstNode var_expr2(ref Parser p, AstNode first) {
    AstNode name_expr, type_expr, value_expr;
    auto span = first.span;

    if (p.consume(Token.Colon)) {
        name_expr = first;
        if (p.consume(Token.Equals)) {
            value_expr = p.primary();
            type_expr = AstNode.none;
            span = span.merge(value_expr.span);
        }
        else {
            // var_expr : Expression (":" Expression)?
            type_expr = p.primary();
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

/// Function := Expression '=>' Expression ;
AstNode function_(ref Parser p, AstNode params) {
    p.consume(Token.FatRArrow);
    auto body = p.expression();
    return make_binary(AstNode.Function, params, body);
}

AstNode dot(ref Parser p, AstNode lhs) {
    assert(matches(lhs.type, AstNode.Name, AstNode.List, AstNode.Call, AstNode.SelfCall));
    p.consume(Token.Dot);

    return make_binary(AstNode.Call, lhs, prefix_parslets[p.current.type](p));
}

/// Call := Expression List
AstNode call(ref Parser p, AstNode lhs) {
    assert(p.current.type == Token.Lparen || p.current.type == Token.Lbracket);
    return make_binary(AstNode.Call, lhs, prefix_parslets[p.current.type](p));
}

AstNode path(ref Parser p, AstNode lhs) {
    assert(matches(lhs.type, AstNode.Name, AstNode.List, AstNode.Call, AstNode.SelfCall, AstNode.Path));
    p.consume(Token.ColonColon);
    return make_binary(AstNode.Path, lhs, prefix_parslets[p.current.type](p));
}

alias negate = unary!(AstNode.Negate, Token.Minus);
alias pointer = unary!(AstNode.Pointer, Token.Star);
alias get_ref = unary!(AstNode.GetRef, Token.Ampersand);

/// Unary := <op> Expression
AstNode unary(AstNode.Type t, Token.Type ttype)(ref Parser p) {
    const start = p.current;
    p.consume(ttype);
    auto expr = p.primary();
    return make_n_ary(t, start.span.merge(expr.span), expr);
}

AstNode self_call(ref Parser p) {
    const span = p.current.span;
    p.consume(Token.Dot);
    auto expr = p.name();
    return make_n_ary(AstNode.SelfCall, span.merge(expr.span), expr);
}

alias assign = binary!(AstNode.Assign, Infix.Assignment + 1, Token.Equals);

alias add = binary!(AstNode.Add, Infix.Sum + 1, Token.Plus);
alias subtract = binary!(AstNode.Subtract, Infix.Sum + 1, Token.Minus);
alias multiply = binary!(AstNode.Multiply, Infix.Product + 1, Token.Star);
alias divide = binary!(AstNode.Divide, Infix.Product + 1, Token.Slash);

alias less = binary!(AstNode.Less, Infix.Comparison + 1, Token.Less);
alias less_equal = binary!(AstNode.LessEqual, Infix.Comparison + 1, Token.LessEqual);
alias greater = binary!(AstNode.Greater, Infix.Comparison + 1, Token.Greater);
alias greater_equal = binary!(AstNode.GreaterEqual, Infix.Comparison + 1, Token.GreaterEqual);
alias equal = binary!(AstNode.Equal, Infix.Comparison + 1, Token.EqualEqual);
alias not_equal = binary!(AstNode.NotEqual, Infix.Comparison + 1, Token.BangEqual);

alias and = binary!(AstNode.And, Infix.Logic + 1, Token.And);
alias or = binary!(AstNode.Or, Infix.Logic + 1, Token.Or);

// not Infix.Power + 1 because we want this to be right-associative
alias power = binary!(AstNode.Power, Infix.Power, Token.Caret);

/// Binary := Expression <op> Expression ;
AstNode binary(AstNode.Type t, int prec, Token.Type ttype)(ref Parser p, AstNode lhs) {
    p.consume(ttype);
    auto rhs = p.expression(cast(Infix.Precedence) prec);
    return make_binary(t, lhs, rhs);
}

AstNode if_(ref Parser p) {
    Span span = p.current.span;
    p.consume(Token.If);

    auto cond = p.expression();
    auto body = p.block();
    span = span.merge(body.span);

    AstNode else_branch = AstNode.none;
    if (p.consume(Token.Else)) {
        else_branch = p.statement();
        span = span.merge(else_branch.span);
    }

    return make_n_ary(AstNode.If, span, cond, body, else_branch);
}

AstNode loop(ref Parser p, ) {
    auto span = p.current.span;
    p.consume(Token.Loop);
    auto body = p.expression();
    return make_n_ary(AstNode.Loop, span.merge(body.span), body);
}
