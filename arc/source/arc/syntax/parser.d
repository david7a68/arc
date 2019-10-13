module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.location: SpannedText, Span;
import arc.syntax.reporter: SyntaxError, SyntaxReporter;

struct Parser {
    import arc.syntax.reporter: SyntaxReporter;
    import arc.stringtable: StringTable;

    ///
    Lexer lexer;
    ///
    SyntaxReporter reporter;

    ///
    this(SpannedText source, StringTable* table, SyntaxReporter reporter) {
        lexer = Lexer(source, table);
        lexer.ready();
        this.reporter = reporter;
    }

    bool empty() { return lexer.matches(Token.Eof); }
}

void expect_mismatch(SyntaxReporter reporter, Token.Type expected, Token encountered) {
    reporter.error(
        SyntaxError.TokenExpectMismatch,
        encountered.span.start,
        "A token of type %s was expected, but a %s was encountered instead.",
        expected,
        encountered.type
    );
}

Token take_required(ref Lexer lexer, Token.Type t, SyntaxReporter reporter) {
    if (lexer.matches(t))
        return lexer.take();
    
    expect_mismatch(reporter, t, lexer.current);
    lexer.advance();
    return Token();
}

void skip_required(ref Lexer lexer, Token.Type t, SyntaxReporter reporter) {
    if (lexer.skip(t))
        return;
    
    expect_mismatch(reporter, t, lexer.current);
    lexer.advance();
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
    p.lexer.push_eol_type(Token.Semicolon);
    
    auto s = () {
        switch (p.lexer.current.type) with (Token.Type) {
        case Def:       return def(p);
        case Break:     return break_(p);
        case Return:    return return_(p);
        case Continue:  return continue_(p);
        default:
            auto e = expression(p);
            p.lexer.skip_required(Token.Semicolon, p.reporter);
            return e;
        }
    } ();
    p.lexer.pop_eol_type();
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
    case Label:
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
    auto span = p.lexer.current.span;
    p.lexer.skip_required(Token.Def, p.reporter);
    
    auto name = p.name!true();
    while (p.lexer.matches(Token.ColonColon))
        name = p.path(name);
    
    auto type = AstNode.none;
    if (!p.lexer.skip(Token.Colon)) {
        p.reporter.error(
            SyntaxError.DefineMissingTypeSpec,
            span.start,
            "The definition of %s must have a type specification",
            p.lexer.source.get_text(name.span),
        );
        type = make_invalid(Span(0, 0));
    }
    else if (!p.lexer.matches(Token.Equals)) {
        type = primary(p);
    }
    
    p.lexer.skip_required(Token.Equals, p.reporter);
    auto value = expression(p);
    p.lexer.skip_required(Token.Semicolon, p.reporter);

    return make_n_ary(AstNode.Define, span.merge(value.span), name, type, value);
}

alias break_ = ctrl_flow!(AstNode.Break, Token.Break, true);
alias return_ = ctrl_flow!(AstNode.Return, Token.Return, true);
alias continue_ = ctrl_flow!(AstNode.Continue, Token.Continue, false);

AstNode ctrl_flow(AstNode.Type t, Token.Type keyword, bool with_value)(ref Parser p) {
    auto span = p.lexer.current.span;
    p.lexer.skip_required(keyword, p.reporter);
    scope(exit) p.lexer.skip_required(Token.Semicolon, p.reporter);

    auto label = AstNode.none;
    if (p.lexer.matches(Token.Label)) {
        label = make_label(p.lexer.current.span, p.lexer.current.key);
        p.lexer.advance();
    }

    static if (with_value) {
        auto value = AstNode.none;
        if (!p.lexer.matches(Token.Semicolon))
            value = expression(p);
        return make_n_ary(t, span, label, value);
    }
    else {
        return make_n_ary(t, span, label);
    }
}


//-----------------------------------//
//            Expressions            //
//-----------------------------------//


alias Prefix = AstNode function(ref Parser);

/// List of functions for parsing tokens that can start expressions
immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p = &null_prefix;
    p[Token.Name] = &name!false;
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
    p[Token.Label] = &label;
    return p;
} ();

AstNode null_prefix(ref Parser p) {
    p.reporter.error(
        SyntaxError.TokenNotAnExpression,
        p.lexer.current.span.start,
        "The token %s cannot start an expression",
        p.lexer.source.get_text(p.lexer.current.span),
    );
    scope(exit) p.lexer.advance();
    return make_invalid(p.lexer.current.span);
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
    p[Token.Lparen]         = Infix(Infix.Primary, &call);
    p[Token.Lbracket]       = Infix(Infix.Primary, &call);
    p[Token.Dot]            = Infix(Infix.Primary, &dot);
    p[Token.Colon]          = Infix(Infix.Assignment, &var_expr2);
    p[Token.ColonColon]     = Infix(Infix.Primary, &path);
    return p;
} ();

bool is_expr_token(Token.Type t) {
    return (prefix_parslets[t] !is &null_prefix) || (infix_parslets[t].parselet !is null);
}

AstNode expression(ref Parser p, Infix.Precedence prec = Infix.Precedence.Assignment) {
    auto expr = prefix_parslets[p.lexer.current.type](p);

    if (expr.type != AstNode.Invalid) {
        while (prec <= infix_parslets[p.lexer.current.type].precedence)
            expr = infix_parslets[p.lexer.current.type].parselet(p, expr);
    }

    return expr;
}

AstNode label(ref Parser p) {
    auto token = p.lexer.take();
    auto label = make_label(token.span, token.key);

    if (prefix_parslets[p.lexer.current.type] !is &null_prefix)
        return make_binary(AstNode.Labeled, label, expression(p));

    return label;
}

AstNode primary(ref Parser p) {
    return expression(p, Infix.Primary);
}

/// Name := ('_' | $a-zA-Z)* ;
AstNode name(bool should_test)(ref Parser p) {
    Token t;
    static if (should_test)
        t = p.lexer.take_required(Token.Name, p.reporter);
    else
        t = p.lexer.take();

    if (t.type == Token.Name)
        return make_name(t.span, t.key);
    else
        return make_invalid(t.span);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
AstNode integer(ref Parser p) {
    import std.conv: to;

    scope(exit) p.lexer.advance();
    return make_int(p.lexer.current.span, p.lexer.current.span.text.to!ulong);
}

AstNode char_(ref Parser p) {
    scope(exit) p.lexer.advance();
    return make_char(p.lexer.take().span);
}

alias paren_list = seq!('(', ')');
alias bracket_list = seq!('[', ']');

AstNode seq(char open, char close)(ref Parser p) {
    AstNode list2(Span span, AstNode first) {
        p.lexer.skip_all(Token.Comma);
        p.lexer.push_eol_type(Token.Comma);

        auto members = [first];
        while(!p.lexer.matches(cast(Token.Type) close)) {
            members ~= var_expr(p);
            p.lexer.skip_all(Token.Comma);

            if (!is_expr_token(p.lexer.current.type) && !p.lexer.matches(cast(Token.Type) close)) {
                scope(exit) p.lexer.advance();
                p.lexer.pop_eol_type();
                p.reporter.error(
                    SyntaxError.SequenceMissingClosingDelimiter,
                    span.start,
                    "List not closed",
                    AstNode.List,
                );
                return make_invalid(span.merge(p.lexer.current.span));
            }
        }

        p.lexer.pop_eol_type();
        span = span.merge(p.lexer.take_required(cast(Token.Type) close, p.reporter).span);
        return make_n_ary(AstNode.List, span, members);
    }

    AstNode list(Span start) {
        p.lexer.skip_all(Token.Comma);
        return list2(start, var_expr(p));
    }

    AstNode wrap(AstNode node) {
        return make_n_ary(AstNode.VarExpression, node.span, [AstNode.none, AstNode.none, node]);
    }

    Span span = p.lexer.current.span;
    p.lexer.skip_required(cast(Token.Type) open, p.reporter);

    if (p.lexer.skip(Token.Comma)) return list(span); // list

    if (!p.lexer.matches(cast(Token.Type) close)) {
        auto next = expression(p);

        if (next.type == AstNode.VarExpression) {
            return list2(span, next);
        }
        else if (p.lexer.skip(Token.Comma)) {
            return list2(span, wrap(next));
        }
        else if (p.lexer.skip(Token.DotDot)) {
            span = span.merge(p.lexer.take_required(cast(Token.Type) close, p.reporter).span);
            return make_n_ary(AstNode.Array, span, next);
        }
        else if (p.lexer.current.type == cast(Token.Type) close) {
            return make_n_ary(AstNode.List, span.merge(p.lexer.take().span), wrap(next));
        }
        else {
            scope(exit) p.lexer.advance();
            p.reporter.error(
                SyntaxError.SequenceMissingClosingDelimiter,
                span.start,
                "List not closed",
                AstNode.List,
            );
            return make_invalid(span.merge(p.lexer.current.span));
        }
    }
    return make_n_ary(AstNode.List, span.merge(p.lexer.take().span));
}

AstNode block(ref Parser p) {
    auto span = p.lexer.current.span.span;
    p.lexer.push_eol_type(Token.Semicolon);
    p.lexer.skip_required(Token.Lbrace, p.reporter);

    p.lexer.skip_all(Token.Semicolon);
    
    AstNode[] members;
    if (!p.lexer.matches(Token.Rbrace)) do {
        auto e = statement(p);
        members ~= e;

        p.lexer.skip_all(Token.Semicolon);

        if (!is_stat_token(p.lexer.current.type) && !p.lexer.matches(Token.Rbrace)) {
            scope(exit) p.lexer.advance();
            p.lexer.pop_eol_type();
            p.reporter.error(
                SyntaxError.SequenceMissingClosingDelimiter,
                span.start,
                "Block not closed",
                AstNode.Block,
            );
            return make_invalid(span.merge(p.lexer.current.span));
        }
    } while (!p.lexer.matches(Token.Rbrace));

    span = span.merge(p.lexer.current.span);
    p.lexer.pop_eol_type();
    p.lexer.take_required(Token.Rbrace, p.reporter);
    return make_n_ary(AstNode.Block, span, members);
}

/**
 * var_expr : Expression ((":" (('=' Expression) | (Expression ("=" Expression)?)) |
 *                        ("=" Expression))?
 */
AstNode var_expr(ref Parser p) {
    return var_expr2(p, primary(p));
}

AstNode var_expr2(ref Parser p, AstNode first) {
    auto span = first.span;

    auto name_expr = AstNode.none;
    auto type_expr = AstNode.none;
    auto value_expr = AstNode.none;
    if (p.lexer.skip(Token.Colon)) {
        name_expr = first;
        if (p.lexer.skip(Token.Equals)) {
            value_expr = primary(p);
            span = span.merge(value_expr.span);
        }
        else {
            // var_expr : Expression (":" Expression)?
            type_expr = primary(p);
            span = span.merge(type_expr.span);

            if (p.lexer.skip(Token.Equals)) {
                // var_expr : Expression (":" Expression ("=" Expression)?)?
                value_expr = expression(p);
                span = span.merge(value_expr.span);
            }
        }
    }
    else if (p.lexer.skip(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        name_expr = first;
        value_expr = expression(p);
        span = span.merge(value_expr.span);
    }
    else {
        // var_expr : Expression
        value_expr = first;
    }

    return make_n_ary(AstNode.VarExpression, span, name_expr, type_expr, value_expr);
}

/// Function := Expression '=>' Expression ;
AstNode function_(ref Parser p, AstNode params) {
    p.lexer.skip_required(Token.FatRArrow, p.reporter);
    auto body = expression(p);
    return make_binary(AstNode.Function, params, body);
}

AstNode dot(ref Parser p, AstNode lhs) {
    assert(matches(lhs.type, AstNode.Name, AstNode.List, AstNode.Call, AstNode.SelfCall));
    p.lexer.skip_required(Token.Dot, p.reporter);

    return make_binary(AstNode.Call, lhs, prefix_parslets[p.lexer.current.type](p));
}

/// Call := Expression List
AstNode call(ref Parser p, AstNode lhs) {
    assert(p.lexer.current.type == Token.Lparen || p.lexer.current.type == Token.Lbracket);
    return make_binary(AstNode.Call, lhs, prefix_parslets[p.lexer.current.type](p));
}

AstNode path(ref Parser p, AstNode lhs) {
    assert(matches(lhs.type, AstNode.Name, AstNode.List, AstNode.Call, AstNode.SelfCall, AstNode.Path));
    p.lexer.skip_required(Token.ColonColon, p.reporter);
    return make_binary(AstNode.Path, lhs, prefix_parslets[p.lexer.current.type](p));
}

alias negate        = unary!(AstNode.Negate, primary);
alias pointer       = unary!(AstNode.Pointer, primary);
alias get_ref       = unary!(AstNode.GetRef, primary);
alias self_call     = unary!(AstNode.SelfCall, name!true);

/// Unary := <op> Expression
AstNode unary(AstNode.Type t, alias next_expr_fn)(ref Parser p) {
    const start = p.lexer.take().span;
    auto expr = next_expr_fn(p);
    return make_n_ary(t, start.merge(expr.span), expr);
}

alias assign        = binary!(AstNode.Assign, Infix.Assignment + 1);

alias add           = binary!(AstNode.Add, Infix.Sum + 1);
alias subtract      = binary!(AstNode.Subtract, Infix.Sum + 1);
alias multiply      = binary!(AstNode.Multiply, Infix.Product + 1);
alias divide        = binary!(AstNode.Divide, Infix.Product + 1);

// not Infix.Power + 1 because we want this to be right-associative
alias power         = binary!(AstNode.Power, Infix.Power);

alias less          = binary!(AstNode.Less, Infix.Comparison + 1);
alias less_equal    = binary!(AstNode.LessEqual, Infix.Comparison + 1);
alias greater       = binary!(AstNode.Greater, Infix.Comparison + 1);
alias greater_equal = binary!(AstNode.GreaterEqual, Infix.Comparison + 1);
alias equal         = binary!(AstNode.Equal, Infix.Comparison + 1);
alias not_equal     = binary!(AstNode.NotEqual, Infix.Comparison + 1);

alias and           = binary!(AstNode.And, Infix.Logic + 1);
alias or            = binary!(AstNode.Or, Infix.Logic + 1);

/// Binary := Expression <op> Expression ;
AstNode binary(AstNode.Type t, int prec)(ref Parser p, AstNode lhs) {
    p.lexer.advance();
    auto rhs = expression(p, cast(Infix.Precedence) prec);
    return make_binary(t, lhs, rhs);
}

AstNode if_(ref Parser p) {
    Span span = p.lexer.current.span;
    p.lexer.skip_required(Token.If, p.reporter);

    auto cond = expression(p);
    auto body = block(p);
    span = span.merge(body.span);

    AstNode else_branch = AstNode.none;
    if (p.lexer.skip(Token.Else)) {
        else_branch = expression(p);
        span = span.merge(else_branch.span);
    }

    return make_n_ary(AstNode.If, span, cond, body, else_branch);
}

AstNode loop(ref Parser p) {
    auto span = p.lexer.current.span;
    p.lexer.skip_required(Token.Loop, p.reporter);
    auto body = expression(p);
    return make_n_ary(AstNode.Loop, span.merge(body.span), body);
}
