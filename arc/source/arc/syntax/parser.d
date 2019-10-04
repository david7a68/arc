module arc.syntax.parser;

import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.location: SpannedText;

struct Parser {
    import arc.syntax.syntax_reporter: SyntaxReporter;
    import arc.stringtable: StringTable;
    import arc.syntax.location: SpannedText;

    ///
    Token token;
    ///
    Lexer lexer;
    ///
    StringTable *table;
    ///
    SyntaxReporter *error;

    ///
    this(SpannedText source, StringTable* table, SyntaxReporter* error) {
        lexer = Lexer(source, table);
        lexer.ready();
        token = lexer.current;
        this.error = error;
    }

    ///
    void advance() {
        lexer.advance();
        token = lexer.current;
        // import std.stdio; writeln(token);
    }

    ///
    bool consume(Token.Type type) {
        if (type != token.type)
            return false;
        
        advance();
        return true;
    }

    /**
     * Push a new token type to the top of the automatic end-of-line token
     * insertion stack
     */
    void push_eol_type(Token.Type type) {
        lexer.push_eol_type(type);
    }

    /**
     * Pop a token type from the top of the automatic end-of-line token
     * insertion stack
     */
    void pop_eol_type() {
        lexer.pop_eol_type();
    }
}

///
Expression expression(ref Parser p) {
    return expression(p, Infix.Precedence.Assignment);
}

alias Prefix = Expression function(ref Parser);

/// List of functions for parsing tokens that can start expressions
immutable Prefix[256] prefix_parslets = () {
    Prefix[256] p;

    p[Token.Lparen] = &list;
    p[Token.Lbracket] = &list;
    p[Token.Name] = &name;
    p[Token.Integer] = &integer;
    p[Token.Minus] = &negate;

    return p;
} ();


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
        Unary,
        Call,
        FunctionLiteral
    }

    Precedence precedence;

    alias InfixParselet = Expression function(ref Parser, Expression);
    InfixParselet parselet;
    
    alias Precedence this;
}

/// List of functions for parsing infix expressions (binary operators, calls, etc)
immutable Infix[256] infix_parslets = () {
    Infix[256] p;

    p[Token.Rarrow]     = Infix(Infix.FunctionLiteral, &function_);
    p[Token.Plus]       = Infix(Infix.Sum, &add);
    p[Token.Minus]      = Infix(Infix.Sum, &subtract);
    p[Token.Star]       = Infix(Infix.Product, &multiply);
    p[Token.Slash]      = Infix(Infix.Product, &divide);
    p[Token.Caret]      = Infix(Infix.Power, &power);
    p[Token.Lparen]     = Infix(Infix.Call, &call);
    p[Token.Lbracket]   = Infix(Infix.Call, &call);

    return p;
} ();

Expression expression(ref Parser p, Infix.Precedence prec) {
    auto parselet = prefix_parslets[p.token.type];

    if (parselet is null) {
        p.error.token_cannot_start_expr(p.token);
        return new Invalid(p.token.span);
    }
    
    auto expr = parselet(p);
    if (expr.type != AstNode.Invalid) {
        while (prec <= infix_parslets[p.token.type].precedence)
            expr = infix_parslets[p.token.type].parselet(p, expr);
    }

    return expr;
}

/// Name := ('_' | $a-zA-Z)* ;
Name name(ref Parser p) {
    scope(exit) p.advance();
    return new Name(p.token.span, p.token.key);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
Integer integer(ref Parser p) {
    import std.conv: to;

    scope(exit) p.advance();
    return new Integer(p.token.span, p.token.span.text.to!ulong);
}

@("parser:int") unittest {
    mixin(parser_init!("10294"));
    const e = parser.expression();
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Integer);
    assert(e.span == text.span);
    assert((cast(Integer) e).value == 10_294);
}

/// List := ListOpen  (','* VarExpression)? ','* ListClose ;
Expression list(ref Parser p) {
    const start = p.token;
    const closing_tok = p.token.type == Token.Lbracket ? Token.Rbracket : Token.Rparen;
    p.push_eol_type(Token.Comma);
    scope(exit) p.pop_eol_type();
    p.advance();

    // skip leading commas
    while(p.token.type == Token.Comma)
        p.advance();

    VarExpression[] members;
    while (p.token.type != closing_tok) {
        auto e = p.var_expr();
        members ~= e;
        
        if (p.token.type == Token.Comma) {
            do {
                p.advance();
            } while (p.token.type == Token.Comma);
        }
        else if (p.token.type != closing_tok) {
            auto span = start.span.merge(p.token.span);
            p.advance();
            p.error.list_not_closed(span, p.token.span);
            return new Invalid(span);
        }
    }

    const close = p.token;
    p.consume(closing_tok);
    return new List(start.span.merge(close.span), members);
}

@("parser:list") unittest {
    {
        mixin(parser_init!("[a\n\n\n(b)]"));
        auto e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.List);
        assert(e.span == text.span);
        assert(e.children.length == 2);
        assert(e.children[0].type == AstNode.VarExpression);
        assert(e.children[1].type == AstNode.VarExpression);
    }
    {
        mixin(parser_init!("[a\n)"));
        bool[2] errors; // [0]: cannot start expr, [1]: list not closed
        parser.error.user_data = &errors;
        parser.error.token_cannot_start_expr_impl = (self, tok) {
            (cast(bool[2]*) self.user_data)[0] = true;
        };
        parser.error.list_not_closed_impl = (self, loc, erloc) {
            (cast(bool[2]*) self.user_data)[1] = true;
        };

        const e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.Invalid);
        assert(e.span == text.span);
        assert(errors[0] && errors[1]);
    }
    {
        mixin(parser_init!("[a, [b)]"));
        bool not_closed;
        parser.error.user_data = &not_closed;
        parser.error.list_not_closed_impl = report_loc_err_set_flag;

        auto e = parser.expression();
        assert(parser.token.type == Token.Eof);
        assert(e.type == AstNode.List);
        assert(e.children[0].type == AstNode.VarExpression);
        assert(e.children[1].type == AstNode.VarExpression);
        assert(e.children[1].children[2].type == AstNode.Invalid);
        assert(e.span == text.span);
        assert(not_closed);
    }
}

/// Negate := '-' Expression
Expression negate(ref Parser p) {
    const start = p.token;
    p.consume(Token.Minus);

    auto expr = prefix_parslets[p.token.type](p);
    if (expr.type != AstNode.Invalid)
        return new Negate(expr, start.span.merge(expr.span));

    return new Invalid(start.span.merge(expr.span));
}

/// Function := Expression '->' Expression ;
Expression function_(ref Parser p, Expression params) {
    p.consume(Token.Rarrow);

    auto body = p.expression();

    if (params.type != AstNode.Invalid && body.type != AstNode.Invalid)
        return new Function(params, body);

    return new Invalid(params.span.merge(body.span));
}

@("parser:function") unittest {
    mixin(parser_init!("() -> ()"));
    auto e = cast(Function) parser.expression();
    assert(parser.token.type == Token.Eof);
    assert(e.parameters.type == AstNode.List);
    assert(e.body.type == AstNode.List);
}

/// Binary := Expression <op> Expression ;
Expression binary(T, int prec, Token.Type ttype)(ref Parser p, Expression lhs) {
    p.consume(ttype);

    auto rhs = p.expression(cast(Infix.Precedence) prec);
    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new T(lhs, rhs);
    return new Invalid(lhs.span.merge(rhs.span));
}

alias add = binary!(Add, Infix.Sum + 1, Token.Plus);
alias subtract = binary!(Subtract, Infix.Sum + 1, Token.Minus);
alias multiply = binary!(Multiply, Infix.Product + 1, Token.Star);
alias divide = binary!(Divide, Infix.Product + 1, Token.Slash);
alias power = binary!(Power, Infix.Power, Token.Caret);

/// Call := Expression List
Expression call(ref Parser p, Expression lhs) {
    auto rhs = p.list();

    if (lhs.type != AstNode.Invalid && rhs.type != AstNode.Invalid)
        return new Call(lhs, rhs);
    return new Invalid(lhs.span.merge(rhs.span));
}

@("parser:call") unittest {
    mixin(parser_init!"[][()]");
    auto e = cast(Call) parser.expression();
    assert(parser.token.type == Token.Eof);
    assert(e.type == AstNode.Call);
    assert(e.children.length == 2);
    assert(e.target.type == AstNode.List);
    assert(e.arguments.type == AstNode.List);
    assert((cast(VarExpression) e.arguments.children[0]).value_expr.type == AstNode.List);
}

/**
 * var_expr : Expression ((":" Expression ("=" Expression)?) |
 *                        ("=" Expression))?
 */
VarExpression var_expr(ref Parser p) {
    Expression first, type_expr, value_expr;

    first = p.expression();
    auto span = first.span;

    if (p.consume(Token.Colon)) {
        // var_expr : Expression (":" Expression)?
        type_expr = p.expression();
        span = span.merge(type_expr.span);

        if (p.consume(Token.Equals)) {
            // var_expr : Expression (":" Expression ("=" Expression)?)?
            value_expr = p.expression();
            span = span.merge(value_expr.span);
        }
    }
    else if (p.consume(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        value_expr = p.expression();
        span = span.merge(value_expr.span);
    }
    else {
        // var_expr : Expression
        value_expr = first;
        first = null;
    }

    return new VarExpression(first, type_expr, value_expr, span);
}

@("parser:var_expr") unittest {
    {
        mixin(parser_init!"a");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.span == text.span);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern is null);
        assert(e.type_expr is null);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        mixin(parser_init!"a:b");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.span == text.span);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr.type == AstNode.Name);
        assert(e.value_expr is null);
    }
    {
        mixin(parser_init!"a:b=c");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.span == text.span);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr.type == AstNode.Name);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        mixin(parser_init!"a=c");
        auto e = cast(VarExpression) parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.span == text.span);
        assert(e.type == AstNode.VarExpression);
        assert(e.pattern.type == AstNode.Name);
        assert(e.type_expr is null);
        assert(e.value_expr.type == AstNode.Name);
    }
    {
        mixin(parser_init!"a:=c");
        bool var_error;
        parser.error.user_data = &var_error;
        parser.error.token_cannot_start_expr_impl = report_bad_token_set_flag;
        
        auto e = parser.var_expr();
        assert(parser.token.type == Token.Eof);
        assert(e.span == text.span);
        assert(var_error);
    }
}

version(unittest) {
    import unit_threaded.should;

    import arc.stringtable: StringTable;
    import arc.syntax.syntax_reporter: SyntaxReporter;

    SyntaxReporter.ReportingFunction_loc_err report_loc_err_set_flag = (r, l1, l2) {
        *(cast(bool*) r.user_data) = true;
    };

    SyntaxReporter.ReportingFunction_loc report_loc_set_flag = (r, l) {
        *(cast(bool*) r.user_data) = true;
    };

    SyntaxReporter.ReportingFunction_token report_bad_token_set_flag = (r, t) {
        *(cast(bool*) r.user_data) = true;
    };

    template parser_init(string s) {
        import std.format: format;
        enum parser_init = "
            StringTable table;
            auto error = SyntaxReporter();
            auto text = SpannedText(0, %s, \"%s\");
            auto parser = Parser(text, &table, &error);
        ".format(s.length, s);
    }
}
