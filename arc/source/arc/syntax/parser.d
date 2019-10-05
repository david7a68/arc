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
        // current = lexer.current;
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
    auto parselet = prefix_parslets[p.current.type];

    if (parselet is null) {
        p.error.token_cannot_start_expr(p.current);
        return new Invalid(p.current.span);
    }
    
    auto expr = parselet(p);
    if (expr.type != AstNode.Invalid) {
        while (prec <= infix_parslets[p.current.type].precedence)
            expr = infix_parslets[p.current.type].parselet(p, expr);
    }

    return expr;
}

/// Name := ('_' | $a-zA-Z)* ;
Name name(ref Parser p) {
    scope(exit) p.advance();
    return new Name(p.current.span, p.current.key);
}

/// Integer := NonZeroDigit ('_' | Digit)* ;
Integer integer(ref Parser p) {
    import std.conv: to;

    scope(exit) p.advance();
    return new Integer(p.current.span, p.current.span.text.to!ulong);
}

@("parser:int") unittest {
    mixin(parser_init!("10294"));
    const expr = cast(Integer) parser.expression();
    mixin(parser_done!());
    assert(expr.type == AstNode.Integer);
    assert(expr.span == text.span);
    assert(expr.value == 10_294);
}

/// List := ListOpen  (','* VarExpression)? ','* ListClose ;
Expression list(ref Parser p) {
    const start = p.current;
    const closing_tok = p.current.type == Token.Lbracket ? Token.Rbracket : Token.Rparen;
    p.push_eol_type(Token.Comma);
    scope(exit) p.pop_eol_type();
    p.advance();

    // skip leading commas
    while(p.current.type == Token.Comma)
        p.advance();

    VarExpression[] members;
    while (p.current.type != closing_tok) {
        auto e = p.var_expr();
        members ~= e;
        
        if (p.current.type == Token.Comma) {
            do {
                p.advance();
            } while (p.current.type == Token.Comma);
        }
        else if (p.current.type != closing_tok) {
            auto span = start.span.merge(p.current.span);
            p.advance();
            p.error.list_not_closed(span, p.current.span);
            return new Invalid(span);
        }
    }

    const close = p.current;
    p.consume(closing_tok);
    return new List(start.span.merge(close.span), members);
}

@("parser:list") unittest {
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

        auto expr = parser.expression();
        mixin(parser_done!());
        assert(diff(expr, Match(AstNode.Invalid)).length == 0);
        assert(errors[0] && errors[1]);
    }
    {
        mixin(parser_init!("[a, [b)]"));
        bool not_closed;
        parser.error.user_data = &not_closed;
        parser.error.list_not_closed_impl = report_loc_err_set_flag;

        auto expr = parser.expression();
        mixin(parser_done!());
        assert(diff(expr, Match(AstNode.List, [
            Match(AstNode.VarExpression, [
                Match(AstNode.None),
                Match(AstNode.None),
                Match(AstNode.Name),
            ]),
            Match(AstNode.VarExpression, [
                Match(AstNode.None),
                Match(AstNode.None),
                Match(AstNode.Invalid),
            ])
        ])).length == 0);
        assert(not_closed);
    }
}

/// Negate := '-' Expression
Expression negate(ref Parser p) {
    const start = p.current;
    p.consume(Token.Minus);
    auto expr = prefix_parslets[p.current.type](p);
    return new Negate(expr, start.span.merge(expr.span));
}

/// Function := Expression '->' Expression ;
Expression function_(ref Parser p, Expression params) {
    p.consume(Token.Rarrow);
    auto body = p.expression();
    return new Function(params, body);
}

@("parser:function") unittest {
    mixin(parser_init!("() -> ()"));
    auto expr = parser.expression();
    mixin(parser_done!());
    assert(diff(expr, Match(AstNode.Function, [
        Match(AstNode.List),
        Match(AstNode.List)
    ])).length == 0);
}

/// Binary := Expression <op> Expression ;
Expression binary(T, int prec, Token.Type ttype)(ref Parser p, Expression lhs) {
    p.consume(ttype);
    auto rhs = p.expression(cast(Infix.Precedence) prec);
    return new T(lhs, rhs);
}

alias add = binary!(Add, Infix.Sum + 1, Token.Plus);
alias subtract = binary!(Subtract, Infix.Sum + 1, Token.Minus);
alias multiply = binary!(Multiply, Infix.Product + 1, Token.Star);
alias divide = binary!(Divide, Infix.Product + 1, Token.Slash);
alias power = binary!(Power, Infix.Power, Token.Caret);

/// Call := Expression List
Expression call(ref Parser p, Expression lhs) {
    return new Call(lhs, p.list());
}

@("parser:call") unittest {
    mixin(parser_init!"[][()]");
    auto expr = cast(Call) parser.expression();
    mixin(parser_done!());
    assert(diff(expr, Match(AstNode.Call, [
        Match(AstNode.List),
        Match(AstNode.List, [
            Match(AstNode.VarExpression, [
                Match(AstNode.None),
                Match(AstNode.None),
                Match(AstNode.List),
            ])
        ])
    ])).length == 0);
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
        else {
            value_expr = None.instance;
        }
    }
    else if (p.consume(Token.Equals)) {
        // var_expr : Expression ("=" Expression)?
        value_expr = p.expression();
        type_expr = None.instance;
        span = span.merge(value_expr.span);
    }
    else {
        // var_expr : Expression
        value_expr = first;
        first = None.instance;
        type_expr = None.instance;
    }

    return new VarExpression(first, type_expr, value_expr, span);
}

@("parser:var_expr") unittest {
    {
        mixin(parser_init!"a");
        auto expr = parser.var_expr();
        mixin(parser_done!());
        assert(diff(expr, Match(AstNode.VarExpression, [
            Match(AstNode.None),
            Match(AstNode.None),
            Match(AstNode.Name)
        ])).length == 0);
    }
    {
        mixin(parser_init!"a:b");
        auto expr = cast(VarExpression) parser.var_expr();
        mixin(parser_done!());
        assert(diff(expr, Match(AstNode.VarExpression, [
            Match(AstNode.Name),
            Match(AstNode.Name),
            Match(AstNode.None)
        ])).length == 0);
    }
    {
        mixin(parser_init!"a:b=c");
        auto expr = cast(VarExpression) parser.var_expr();
        mixin(parser_done!());
        assert(diff(expr, Match(AstNode.VarExpression, [
            Match(AstNode.Name),
            Match(AstNode.Name),
            Match(AstNode.Name),
        ])));
    }
    {
        mixin(parser_init!"a=c");
        auto expr = cast(VarExpression) parser.var_expr();
        mixin(parser_done!());
        assert(diff(expr, Match(AstNode.VarExpression, [
            Match(AstNode.Name),
            Match(AstNode.None),
            Match(AstNode.Name),
        ])));
    }
    {
        mixin(parser_init!"a:=1");
        bool var_error;
        parser.error.user_data = &var_error;
        parser.error.token_cannot_start_expr_impl = report_bad_token_set_flag;
        
        auto expr = parser.var_expr();
        mixin(parser_done!());
        assert(var_error);
        assert(diff(expr, Match(AstNode.VarExpression, [
            Match(AstNode.Name),
            Match(AstNode.Invalid),
            Match(AstNode.Integer)
        ])).length == 0);
    }
}

version(unittest) {
    import unit_threaded.should;

    import arc.stringtable: StringTable;
    import arc.syntax.syntax_reporter: SyntaxReporter;

    SyntaxReporter.ReportingFunction_loc_err report_loc_err_set_flag = (r, l1, l2) {
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

    template parser_done() {
        enum parser_done = "parser.empty.should == true; expr.span.should == text.span;";
    }

    struct Match {
        AstNode.Type node_type;
        Match[] children;
    }

    AstNode[] diff(AstNode root, Match match) {
        AstNode[] r;
        if (root.type != match.node_type || root.children.length != match.children.length)
            r ~= root;
        else {
            foreach (i, child; root.children)
                r ~= diff(child, match.children[i]);
        }
        return r;
    }
}
