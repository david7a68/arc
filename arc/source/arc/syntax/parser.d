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


AstNode* parse_statement(ref Parser p) {
    p.lexer.push_eol_type(Token.Semicolon);
    
    auto s = () {
        switch (p.lexer.current.type) {
        case Token.Def:       return parse_def(p);
        case Token.Break:     return ctrl_flow!(Break, Token.Break, true)(p);
        case Token.Return:    return ctrl_flow!(Return, Token.Return, true)(p);
        case Token.Continue:  return ctrl_flow!(Continue, Token.Continue, false)(p);
        default:
            auto e = parse_expression(p);
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
AstNode* parse_def(ref Parser p) {
    auto span = p.lexer.current.span;
    p.lexer.skip_required(Token.Def, p.reporter);
    
    auto name = parse_name(p);
    while (p.lexer.matches(Token.ColonColon))
        name = parse_path(p, name);
    
    auto type = AstNode.none;
    if (!p.lexer.skip(Token.Colon)) {
        p.reporter.error(
            SyntaxError.DefineMissingTypeSpec,
            span.start,
            "The definition of %s must have a type specification",
            p.lexer.source.get_text(name.span),
        );
        type = make!Invalid(Span(0, 0));
    }
    else if (!p.lexer.matches(Token.Equals)) {
        type = parse_primary(p);
    }
    
    p.lexer.skip_required(Token.Equals, p.reporter);
    auto value = parse_expression(p);
    p.lexer.skip_required(Token.Semicolon, p.reporter);

    return make!Define(span.merge(value.span), name, type, value);
}

AstNode* ctrl_flow(T, Token.Type keyword, bool with_value)(ref Parser p) {
    auto span = p.lexer.current.span;
    p.lexer.skip_required(keyword, p.reporter);
    scope(exit) p.lexer.skip_required(Token.Semicolon, p.reporter);

    auto label = AstNode.none;
    if (p.lexer.matches(Token.Label)) {
        label = make!Label(p.lexer.current.span, p.lexer.current.key);
        p.lexer.advance();
    }

    static if (with_value) {
        auto value = AstNode.none;
        if (!p.lexer.matches(Token.Semicolon))
            value = parse_expression(p);
        return make!T(span, [label, value]);
    }
    else {
        return make!T(span, label);
    }
}


//-----------------------------------//
//            Expressions            //
//-----------------------------------//


alias Prefix = AstNode* function(ref Parser);

/// List of functions for parsing tokens that can start expressions
immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p       = &parse_null_prefix;
    p[Token.Name]       = &parse_name;
    p[Token.Integer]    = &parse_integer;
    p[Token.Char]       = &parse_char;
    p[Token.Lparen]     = &parse_paren_list;
    p[Token.Lbracket]   = &parse_bracket_list;
    p[Token.Lbrace]     = &parse_block;
    p[Token.Label]      = &parse_label;
    p[Token.If]         = &parse_if;
    p[Token.Loop]       = &parse_loop;
    p[Token.Minus]      = &parse_unary!Negate;
    p[Token.Ampersand]  = &parse_unary!GetRef;
    p[Token.Star]       = &parse_unary!Pointer;
    return p;
} ();

AstNode* parse_null_prefix(ref Parser p) {
    p.reporter.error(
        SyntaxError.TokenNotAnExpression,
        p.lexer.current.span.start,
        "The token %s cannot start an expression",
        p.lexer.source.get_text(p.lexer.current.span),
    );
    scope(exit) p.lexer.advance();
    return make!Invalid(p.lexer.current.span);
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

    alias InfixParselet = AstNode* function(ref Parser, AstNode*);
    InfixParselet parselet;
    
    alias Precedence this;
}

/// List of functions for parsing infix expressions (binary operators, calls, etc)
immutable Infix[256] infix_parslets = () {
    Infix[256] p;
    p[Token.Plus]           = Infix(Infix.Sum,          &parse_binary!(Add, Infix.Sum + 1));
    p[Token.Minus]          = Infix(Infix.Sum,          &parse_binary!(Subtract, Infix.Sum + 1));
    p[Token.Star]           = Infix(Infix.Product,      &parse_binary!(Multiply, Infix.Product + 1));
    p[Token.Slash]          = Infix(Infix.Product,      &parse_binary!(Divide, Infix.Product + 1));
    p[Token.Caret]          = Infix(Infix.Power,        &parse_binary!(Power, Infix.Power));
    p[Token.Less]           = Infix(Infix.Comparison,   &parse_binary!(Less, Infix.Comparison + 1));
    p[Token.LessEqual]      = Infix(Infix.Comparison,   &parse_binary!(LessEqual, Infix.Comparison + 1));
    p[Token.Greater]        = Infix(Infix.Comparison,   &parse_binary!(Greater, Infix.Comparison + 1));
    p[Token.GreaterEqual]   = Infix(Infix.Comparison,   &parse_binary!(GreaterEqual, Infix.Comparison + 1));
    p[Token.EqualEqual]     = Infix(Infix.Equality,     &parse_binary!(Equal, Infix.Equality + 1));
    p[Token.BangEqual]      = Infix(Infix.Equality,     &parse_binary!(NotEqual, Infix.Equality + 1));
    p[Token.Equals]         = Infix(Infix.Assignment,   &parse_binary!(Assign, Infix.Assignment + 1));
    p[Token.And]            = Infix(Infix.Logic,        &parse_binary!(And, Infix.Logic + 1));
    p[Token.Or]             = Infix(Infix.Logic,        &parse_binary!(Or, Infix.Logic + 1));
    
    p[Token.FatRArrow]      = Infix(Infix.Primary,      &parse_function);
    p[Token.Lparen]         = Infix(Infix.Primary,      &parse_binary!(Call, Infix.Primary + 1, false));
    p[Token.Lbracket]       = Infix(Infix.Primary,      &parse_binary!(Call, Infix.Primary + 1, false));
    p[Token.Dot]            = Infix(Infix.Primary,      &parse_binary!(Call, Infix.Primary + 1));
    p[Token.Colon]          = Infix(Infix.Assignment,   &parse_var_expr2);
    p[Token.ColonColon]     = Infix(Infix.Primary,      &parse_path);
    return p;
} ();

AstNode* parse_expression(ref Parser p, Infix.Precedence prec = Infix.Precedence.Assignment) {
    auto expr = prefix_parslets[p.lexer.current.type](p);

    if (expr.type != AstNode.Invalid) {
        while (prec <= infix_parslets[p.lexer.current.type].precedence)
            expr = infix_parslets[p.lexer.current.type].parselet(p, expr);
    }

    return expr;
}

AstNode* parse_label(ref Parser p) {
    auto token = p.lexer.take();
    auto label = make!Label(token.span, token.key);

    if (prefix_parslets[p.lexer.current.type] !is &parse_null_prefix) {
        auto expr = parse_expression(p);
        return make!Labeled(label.span.merge(expr.span), label, expr);
    }

    return label;
}

AstNode* parse_primary(ref Parser p) {
    return parse_expression(p, Infix.Primary);
}

/// Name := ('_' | $a-zA-Z)* ;
AstNode* parse_name(ref Parser p) {
    auto t = p.lexer.take_required(Token.Name, p.reporter);

    if (t.type == Token.Name)
        return make!Name(t.span, t.key);
    else
        return make!Invalid(t.span);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
AstNode* parse_integer(ref Parser p) {
    import std.conv: to;

    scope(exit) p.lexer.advance();
    return make!Integer(p.lexer.current.span, p.lexer.current.span.text.to!ulong);
}

AstNode* parse_char(ref Parser p) {
    scope(exit) p.lexer.advance();
    auto t = p.lexer.take;
    return make!Char(t.span, t.key);
}

alias parse_paren_list = parse_seq!('(', ')');
alias parse_bracket_list = parse_seq!('[', ']');

AstNode* parse_seq(char open, char close)(ref Parser p) {
    AstNode* list2(Span span, AstNode* first) {
        p.lexer.skip_all(Token.Comma);
        p.lexer.push_eol_type(Token.Comma);

        auto members = [first];
        while(!p.lexer.matches(cast(Token.Type) close)) {
            members ~= parse_var_expr(p);
            p.lexer.skip_all(Token.Comma);

            const is_expr_token = prefix_parslets[p.lexer.current.type] !is &parse_null_prefix;
            if (!is_expr_token && !p.lexer.matches(cast(Token.Type) close)) {
                scope(exit) p.lexer.advance();
                p.lexer.pop_eol_type();
                p.reporter.error(
                    SyntaxError.SequenceMissingClosingDelimiter,
                    span.start,
                    "List not closed",
                    AstNode.List,
                );
                return make!Invalid(span.merge(p.lexer.current.span));
            }
        }

        p.lexer.pop_eol_type();
        span = span.merge(p.lexer.take_required(cast(Token.Type) close, p.reporter).span);
        return make!List(span, members);
    }

    AstNode* list(Span start) {
        p.lexer.skip_all(Token.Comma);
        return list2(start, parse_var_expr(p));
    }

    AstNode* wrap(AstNode* node) {
        return make!VarExpression(node.span, AstNode.none, AstNode.none, node);
    }

    Span span = p.lexer.current.span;
    p.lexer.skip_required(cast(Token.Type) open, p.reporter);

    if (p.lexer.skip(Token.Comma)) return list(span); // list

    if (!p.lexer.matches(cast(Token.Type) close)) {
        auto next = parse_expression(p);

        if (next.type == AstNode.VarExpression) {
            return list2(span, next);
        }
        else if (p.lexer.skip(Token.Comma)) {
            return list2(span, wrap(next));
        }
        else if (p.lexer.skip(Token.DotDot)) {
            span = span.merge(p.lexer.take_required(cast(Token.Type) close, p.reporter).span);
            return make!Array(span, [next]);
        }
        else if (p.lexer.current.type == cast(Token.Type) close) {
            return make!List(span.merge(p.lexer.take().span), [wrap(next)]);
        }
        else {
            scope(exit) p.lexer.advance();
            p.reporter.error(
                SyntaxError.SequenceMissingClosingDelimiter,
                span.start,
                "List not closed",
                AstNode.List,
            );
            return make!Invalid(span.merge(p.lexer.current.span));
        }
    }
    return make!List(span.merge(p.lexer.take().span));
}

AstNode* parse_block(ref Parser p) {
    auto span = p.lexer.current.span.span;
    p.lexer.push_eol_type(Token.Semicolon);
    p.lexer.skip_required(Token.Lbrace, p.reporter);

    p.lexer.skip_all(Token.Semicolon);
    
    AstNode*[] members;
    while (!p.lexer.matches(Token.Rbrace)) {
        members ~= parse_statement(p);
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
            return make!Invalid(span.merge(p.lexer.current.span));
        }
    }

    p.lexer.pop_eol_type();
    span = span.merge(p.lexer.take_required(Token.Rbrace, p.reporter).span);
    return make!Block(span, members);
}

/**
 * var_expr : Expression ((":" (('=' Expression) | (Expression ("=" Expression)?)) |
 *                        ("=" Expression))?
 */
AstNode* parse_var_expr(ref Parser p) {
    return parse_var_expr2(p, parse_primary(p));
}

AstNode* parse_var_expr2(ref Parser p, AstNode* first) {
    auto span = first.span;

    auto name_expr = AstNode.none;
    auto type_expr = AstNode.none;
    auto value_expr = AstNode.none;
    if (p.lexer.skip(Token.Colon)) {
        name_expr = first;
        if (p.lexer.skip(Token.Equals)) {
            value_expr = parse_primary(p);
            span = span.merge(value_expr.span);
        }
        else {
            // var_expr : Expression (":" Expression)?
            type_expr = parse_primary(p);
            span = span.merge(type_expr.span);

            if (p.lexer.skip(Token.Equals)) {
                // var_expr : Expression (":" Expression ("=" Expression)?)?
                value_expr = parse_expression(p);
                span = span.merge(value_expr.span);
            }
        }
    }
    else if (p.lexer.skip(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        name_expr = first;
        value_expr = parse_expression(p);
        span = span.merge(value_expr.span);
    }
    else {
        // var_expr : Expression
        value_expr = first;
    }

    return make!VarExpression(span, name_expr, type_expr, value_expr);
}

/// Function := Expression '=>' Expression ;
AstNode* parse_function(ref Parser p, AstNode* params) {
    p.lexer.skip_required(Token.FatRArrow, p.reporter);
    auto body = parse_expression(p);
    return make!Function(params.span.merge(body.span), params, body);
}

AstNode* parse_path(ref Parser p, AstNode* lhs) {
    assert(matches(lhs.type, AstNode.Name, AstNode.List, AstNode.Call, AstNode.Path));
    p.lexer.skip_required(Token.ColonColon, p.reporter);
    auto rhs = prefix_parslets[p.lexer.current.type](p);
    return make!Path(lhs.span.merge(rhs.span), lhs, rhs);
}

/// Unary := <op> Expression
AstNode* parse_unary(T)(ref Parser p) {
    const start = p.lexer.take().span;
    auto expr = parse_primary(p);
    return make!T(start.merge(expr.span), expr);
}

/// Binary := Expression <op> Expression ;
AstNode* parse_binary(T, int prec, bool skip = true)(ref Parser p, AstNode* lhs) {
    static if (skip) { p.lexer.advance(); }
    auto rhs = parse_expression(p, cast(Infix.Precedence) prec);
    return make!T(lhs.span.merge(rhs.span), lhs, rhs);
}

AstNode* parse_if(ref Parser p) {
    Span span = p.lexer.current.span;
    p.lexer.skip_required(Token.If, p.reporter);

    auto cond = parse_expression(p);
    auto body = parse_block(p);
    span = span.merge(body.span);

    AstNode* else_branch = AstNode.none;
    if (p.lexer.skip(Token.Else)) {
        else_branch = parse_expression(p);
        span = span.merge(else_branch.span);
    }

    return make!If(span, cond, body, else_branch);
}

AstNode* parse_loop(ref Parser p) {
    auto span = p.lexer.current.span;
    p.lexer.skip_required(Token.Loop, p.reporter);
    auto body = parse_block(p);
    return make!Loop(span.merge(body.span), body);
}
