module arc.syntax.syntax_reporter;

import arc.syntax.location: CharPos, Span, Source, SpannedText;

struct SyntaxReporter {
    import std.format: format;
    import std.stdio: writeln;

    import arc.hash: Key;
    import arc.syntax.lexer: Token;
    import arc.syntax.ast: AstNode;

    void* user_data;
    Source source;

    alias ExprLoc = Span;
    alias ErrorLoc = Span;

    /// Function type that reports the location of the error, and the location of its enclosing expression
    alias ReportingFunction_loc_err = void function(SyntaxReporter*, ExprLoc, ErrorLoc);
    alias ReportingFunction_loc_err_type = void function(SyntaxReporter*, ExprLoc, ErrorLoc, AstNode.Type);
    /// Function type that reports the location of the error
    alias ReportingFunction_loc = void function(SyntaxReporter*, ErrorLoc);
    /// Function type that reports a particular token
    alias ReportingFunction_token = void function(SyntaxReporter*, Token);
    /// Function type that reporst the start of the expression, and the token
    alias ReportingFunction_loc_token = void function(SyntaxReporter*, ExprLoc, Token);

    /**
     * Quality of life dispatcher that lets us pretend that the error handlers
     * are methods.
     *
     * To call an error reporting function, such as tuple_missing_comma, simply
     * call `reporter.tuple_missing_comma(expr, loc)`. The reporter parameter
     * will be provided automatically.
     */
    template opDispatch(string s) {
        void opDispatch(Args...)(Args args) {
            mixin(s ~ "_impl(&this, args);");
        }
    }

    /**
     * Reports the unexpected end of the file while parsing the syntactical
     * artifact starting at 'loc'
     */
    ReportingFunction_loc unexpected_end_of_file_impl = (reporter, loc) {
        // assert(false, "Error: Unexpected end of file!");
        assert(false);
    };

    ReportingFunction_loc_err definition_missing_colon_impl = (reporter, span, pos) {
        auto stat_loc = reporter.source.get_loc(span.start);
        auto err_pos = reporter.source.get_loc(pos.start);
        const s = "Error: The definition at (line: %s, column: %s) must have a type specification.".format(
            err_pos.line,
            err_pos.column
        );
        writeln(s);
        writeln("    To automatically infer the type specification, insert a colon (:) after the name.");
        writeln("        Grammar: 'def' path ':' primary? '=' expr ';' ;");
    };

    ReportingFunction_loc_err_type seq_not_closed_impl = (reporter, expr_loc, err_loc, type) {
        auto exp_loc = reporter.source.get_loc(expr_loc.start);
        auto err_pos = reporter.source.get_loc(err_loc.start);
        const s = "Error: The %s at (line: %s, column: %s) is not closed:\n%s (line: %s, column: %s)".format(
            type,
            exp_loc.line,
            exp_loc.column,
            reporter.source.name,
            err_pos.line,
            err_pos.column
        );
        writeln(s);
    }; 

    /**
     * Reports that the described token cannot start an expression
     */
    ReportingFunction_token token_cannot_start_expr_impl = (reporter, token) {
        auto loc = reporter.source.get_loc(token.span.start);
        const s = "Error: The token %s(%s) cannot start an expression at:\n%s (line: %s, column: %s)".format(
            token.type,
            reporter.source.get_text(token.span),
            reporter.source.name,
            loc.line,
            loc.column
        );
        writeln(s);
    };

    /**
     * Reports that the token is not an operator (cannot be in infix position).
     */
    ReportingFunction_loc_token token_is_not_an_operator_impl = (reporter, loc, token) {
        assert(false);
        // assert(false, "Error: The token (" ~ token.start[0 .. token.span] ~ ") is not an operator");
    };
}

struct SourceLoc {
    uint line;
    uint line_position;
    uint column;
}

SourceLoc get_loc(SpannedText source, CharPos position) {
    uint line_num = 1;
    uint line_pos;
    uint column;

    const slice = source.text[0 .. position - source.start];
    foreach (i, c; slice) {
        if (c == '\n') {
            column = 0;
            line_num++;
            line_pos = cast(uint) i;
        }
        column++;
    }

    return SourceLoc(line_num, line_pos, column);
}
