module arc.syntax.syntax_reporter;

import arc.syntax.location: CharPos, Span, Source, SpannedText;

struct SyntaxReporter {
    import std.format: format;
    import std.stdio: writeln;

    import arc.hash: Key;
    import arc.syntax.lexer: Token;

    void* user_data;
    Source source;

    alias ExprLoc = Span;
    alias ErrorLoc = Span;

    /// Function type that reports the location of the error, and the location of its enclosing expression
    alias ReportingFunction_loc_err = void function(SyntaxReporter*, ExprLoc, ErrorLoc);
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
    };

    /**
     * Reports that the list does not have a matching closing token. Either the
     * end of the file has been reached unexpectedly, or the next token is not a
     * comma or closing token.
     */
    ReportingFunction_loc_err list_not_closed_impl = (reporter, expr_loc, err_loc) {
        auto exp_loc = reporter.source.get_loc(expr_loc.start);
        auto err_pos = reporter.source.get_loc(err_loc.start);
        const s = "Error: The list at (line: %s, column: %s) is not closed:\n%s (line: %s, column: %s)".format(
            exp_loc.line,
            exp_loc.column,
            reporter.source.name,
            err_pos.line,
            err_pos.column
        );
        writeln(s);
    }; 
    
    ///
    ReportingFunction_loc_err list_missing_comma_impl = (reporter, expr_loc, err_loc) {
        assert(false, "Error: The list is missing a comma here...");
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
