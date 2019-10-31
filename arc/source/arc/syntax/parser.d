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
    uint count;

    ///
    this(SpannedText source, StringTable* table, SyntaxReporter reporter) {
        lexer = Lexer(source, table);
        lexer.ready();
        this.reporter = reporter;
    }

    /// Shortcut check if the lexer has exhausted all tokens
    bool empty() { return lexer.current.type == Token.Eof; }

    AstNode* make(T, Args...)(Span span, Args args) {
        count++;
        return cast(AstNode*) new T(span, args);
    }

    AstNode* make(T: None)() {
        static immutable none_value = AstNode(AstNode.None, Span());
        
        count++;
        return cast(AstNode*) &none_value;
    }
}

/**
 * Utility function. This raises an error if a token does not match its expected type.
 */
void expect_mismatch(SyntaxReporter reporter, Token.Type expected, Token encountered) {
    reporter.error(
        SyntaxError.TokenExpectMismatch,
        encountered.span.start,
        "A token of type %s was expected, but a %s was encountered instead.",
        expected,
        encountered.type
    );
}

/**
 * Takes a token, enforcing that the type of the token is as expected.
 *
 * This function is here in parser.d instead of lexer.d because it requires
 * access to a SyntaxReporter.
 */
Token take_required(ref Lexer lexer, Token.Type t, SyntaxReporter reporter) {
    if (lexer.current.type == t)
        return lexer.take();
    
    expect_mismatch(reporter, t, lexer.current);
    lexer.advance();
    return Token();
}

/**
 * Skips a token, enforcing that the type of the token is as expected.
 *
 * This function is here in parser.d instead of lexer.d because it requires
 * access to a SyntaxReporter
 */
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
        case Token.Break:     return parse_ctrl_flow!(Break, Token.Break, true)(p);
        case Token.Return:    return parse_ctrl_flow!(Return, Token.Return, true)(p);
        case Token.Continue:  return parse_ctrl_flow!(Continue, Token.Continue, false)(p);
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
    auto is_keyword = () {
        switch (t) with (Token.Type) {
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
    }();

    return is_keyword || can_start_expression(t);
}

/// def = 'def' name ':' primary? '=' expr ;
AstNode* parse_def(ref Parser p) {
    auto span = p.lexer.take_required(Token.Def, p.reporter).span;
    
    auto name = parse_name(p);
    while (p.lexer.current.type == Token.ColonColon)
        name = parse_path(p, name);
    
    AstNode* type;
    if (p.lexer.skip(Token.Colon)) {
        type = p.lexer.current.type != Token.Equals ?
               parse_primary(p) :
               p.make!None;
    }
    else {
        p.reporter.error(
            SyntaxError.DefineMissingTypeSpec,
            span.start,
            "The definition of %s must have a type specification",
            p.lexer.source.get_text(name.span),
        );
        type = p.make!Invalid(Span(0, 0));
    }

    p.lexer.skip_required(Token.Equals, p.reporter);
    auto value = parse_expression(p);
    p.lexer.skip_required(Token.Semicolon, p.reporter);

    return p.make!Define(span.merge(value.span), name, type, value);
}

AstNode* parse_ctrl_flow(T, Token.Type keyword, bool with_value)(ref Parser p) {
    auto span = p.lexer.take_required(keyword, p.reporter).span;
    scope(exit) p.lexer.skip_required(Token.Semicolon, p.reporter);

    auto label = p.lexer.current.type == Token.Label ?
                 parse_label!false(p) :
                 p.make!None();

    static if (with_value) {
        auto value = p.lexer.current.type != Token.Semicolon ?
                     parse_expression(p) :
                     p.make!None;
        return p.make!T(span, [label, value]);
    }
    else {
        return p.make!T(span, label);
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
    p[Token.Label]      = &parse_label!true;
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
    return p.make!Invalid(p.lexer.current.span);
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

    while (prec <= infix_parslets[p.lexer.current.type].precedence)
        expr = infix_parslets[p.lexer.current.type].parselet(p, expr);

    return expr;
}

bool can_start_expression(Token.Type t) {
    return prefix_parslets[t] !is &parse_null_prefix;
}

AstNode* parse_label(bool with_expr)(ref Parser p) {
    auto token = p.lexer.take();
    auto label = p.make!Label(token.span, token.key);

    static if (with_expr) {
        if (can_start_expression(p.lexer.current.type)) {
            auto expr = parse_expression(p);
            return p.make!Labeled(label.span.merge(expr.span), label, expr);
        }
    }

    return label;
}

AstNode* parse_primary(ref Parser p) {
    return parse_expression(p, Infix.Primary);
}

/// Name := ('_' | $a-zA-Z)* ;
AstNode* parse_name(ref Parser p) {
    auto t = p.lexer.take_required(Token.Name, p.reporter);
    return p.make!Name(t.span, t.key);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
AstNode* parse_integer(ref Parser p) {
    import std.conv: to;

    auto t = p.lexer.take_required(Token.Integer, p.reporter);
    return p.make!Integer(t.span, t.span.text.to!ulong);
}

AstNode* parse_char(ref Parser p) {
    auto t = p.lexer.take_required(Token.Char, p.reporter);
    return p.make!Char(t.span, t.key);
}

alias parse_paren_list = parse_seq!(Token.Lparen, Token.Rparen);
alias parse_bracket_list = parse_seq!(Token.Lbracket, Token.Rbracket);

AstNode* parse_seq(Token.Type open, Token.Type close)(ref Parser p) {
    static fast_forward(ref Parser p) {
        Span span = p.lexer.take().span.span;

        if (!p.lexer.matches_one(Token.Comma, Token.Semicolon, close)) {
            int levels = 0;
            while (!p.empty && levels >= 0) {
                if (p.lexer.current.type == Token.Semicolon)
                    return span;
                else if (levels == 0 && p.lexer.current.type == Token.Comma)
                    return span;
                else if (p.lexer.matches_one(Token.Lparen, Token.Lbracket))
                    levels++;
                else if (p.lexer.matches_one(Token.Rparen, Token.Rbracket))
                    levels--;

                span = span.merge(p.lexer.take().span);
            }
        }
        return span;
    }

    static parse_member(ref Parser p) {
        auto first = parse_expression(p, Infix.Logic);

        const is_name = first.type == AstNode.Name;
        auto name = is_name && p.lexer.matches_one(Token.Equals, Token.Colon) ?
                    first :
                    p.make!None;

        auto type = p.lexer.skip(Token.Colon) ?
                    parse_primary(p) :
                    p.make!None;

        auto value = p.lexer.skip(Token.Equals) ?
                    parse_expression(p) :
                    name.type == AstNode.None ?
                        first :
                        p.make!None;

        auto span = name.span.merge(type.span).merge(value.span);
        assert(name && type && value);
        return p.make!ListMember(span, name, type, value);
    }
    
    static report_not_closed(ref Parser p, Span span) {
        p.reporter.error(
            SyntaxError.SequenceMissingClosingDelimiter,
            span.start,
            "List not closed"
        );

        p.lexer.pop_eol_type();
        span = span.merge(fast_forward(p));
        return p.make!Invalid(span);
    }

    p.lexer.push_eol_type(Token.Comma);
    Span span = p.lexer.take_required(open, p.reporter).span;
    AstNode*[] members;

    p.lexer.skip_all(Token.Comma);
    while(!p.empty && p.lexer.current.type != close) {
        auto member = parse_member(p);

        if (p.lexer.current.type == Token.Comma) {
            p.lexer.skip_all(Token.Comma);
        }
        else if (can_start_expression(p.lexer.current.type)) {
            p.reporter.error(
                SyntaxError.SequenceMissingSeparator,
                span.start + span.length,
                "List is missing comma"
            );

            member = p.make!Invalid(member.span.merge(fast_forward(p)));
            p.lexer.skip_all(Token.Comma);

            if (p.lexer.current.type != close && !can_start_expression(p.lexer.current.type))
                return report_not_closed(p, span);
        }
        else if (p.lexer.current.type != close) {
            return report_not_closed(p, span);
        }

        members ~= member;
        span = span.merge(member.span);
    }

    if (p.empty)
        return report_not_closed(p, span);

    p.lexer.pop_eol_type();
    span = span.merge(p.lexer.take_required(close, p.reporter).span);
    return p.make!List(span, members);
}

AstNode* parse_block(ref Parser p) {
    p.lexer.push_eol_type(Token.Semicolon);
    Span span = p.lexer.take_required(Token.Lbrace, p.reporter).span;
    p.lexer.skip_all(Token.Semicolon);
    
    AstNode*[] members;
    while (p.lexer.current.type != Token.Rbrace) {
        members ~= parse_statement(p);
        p.lexer.skip_all(Token.Semicolon);

        if (!is_stat_token(p.lexer.current.type) && p.lexer.current.type != Token.Rbrace) {
            scope(exit) p.lexer.advance();
            p.lexer.pop_eol_type();
            p.reporter.error(
                SyntaxError.SequenceMissingClosingDelimiter,
                span.start,
                "Block not closed",
                AstNode.Block,
            );
            return p.make!Invalid(span.merge(p.lexer.current.span));
        }
    }

    p.lexer.pop_eol_type();
    span = span.merge(p.lexer.take_required(Token.Rbrace, p.reporter).span);
    return p.make!Block(span, members);
}

/**
 * var_expr : Expression ((":" (('=' Expression) | (Expression ("=" Expression)?)) |
 *                        ("=" Expression))?
 */
AstNode* parse_var_expr(ref Parser p) {
    return parse_var_expr2(p, parse_primary(p));
}

AstNode* parse_var_expr2(ref Parser p, AstNode* first) {
    // @cleanup: this needs to become an error
    assert(first.type == AstNode.Name);
    auto name = first;
    p.lexer.take_required(Token.Colon, p.reporter);

    auto type = p.lexer.current.type != Token.Equals ?
                parse_primary(p) :
                p.make!None;

    auto value = p.lexer.skip(Token.Equals) ?
                 parse_expression(p) :
                 p.make!None;

    if (type.type == AstNode.None && value.type == AstNode.None) {
        // @cleanup: this needs to become an error
        assert(false);
    }

    auto span = name.span.merge(type.span).merge(value.span);
    return p.make!VarExpression(span, name, type, value);
}

/// Function := Expression '=>' Expression ;
AstNode* parse_function(ref Parser p, AstNode* params) {
    p.lexer.skip_required(Token.FatRArrow, p.reporter);
    auto body = parse_expression(p);
    return p.make!Function(params.span.merge(body.span), params, body);
}

AstNode* parse_path(ref Parser p, AstNode* lhs) {
    assert(matches(lhs.type, AstNode.Name, AstNode.List, AstNode.Call, AstNode.Path));
    p.lexer.skip_required(Token.ColonColon, p.reporter);
    auto rhs = prefix_parslets[p.lexer.current.type](p);
    return p.make!Path(lhs.span.merge(rhs.span), lhs, rhs);
}

/// Unary := <op> Expression
AstNode* parse_unary(T)(ref Parser p) {
    const start = p.lexer.take().span;
    auto expr = parse_primary(p);
    return p.make!T(start.merge(expr.span), expr);
}

/// Binary := Expression <op> Expression ;
AstNode* parse_binary(T, int prec, bool skip = true)(ref Parser p, AstNode* lhs) {
    static if (skip) { p.lexer.advance(); }
    auto rhs = parse_expression(p, cast(Infix.Precedence) prec);
    return p.make!T(lhs.span.merge(rhs.span), lhs, rhs);
}

AstNode* parse_if(ref Parser p) {
    Span span = p.lexer.take_required(Token.If, p.reporter).span;

    auto cond = parse_expression(p);
    auto body = parse_block(p);

    auto else_branch = p.lexer.skip(Token.Else) ?
                       parse_expression(p) :
                       p.make!None;

    span = span.merge(body.span).merge(else_branch.span);
    return p.make!If(span, cond, body, else_branch);
}

AstNode* parse_loop(ref Parser p) {
    Span span = p.lexer.take_required(Token.Loop, p.reporter).span;
    auto body = parse_block(p);
    return p.make!Loop(span.merge(body.span), body);
}
