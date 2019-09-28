module arc.syntax.syntax_reporter;

struct SyntaxReporter {
    import arc.hash: Key;
    import arc.syntax.lexer: Token;

    Key name, source;
    void* user_data;

    alias ExprLoc = const(char)*;
    alias ErrorLoc = const(char)*;

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
        assert(false, "Error: Unexpected end of file!");
    };

    /**
     * Reports that the list does not have a matching closing token. Either the
     * end of the file has been reached unexpectedly, or the next token is not a
     * comma or closing token.
     */
    ReportingFunction_loc_err list_not_closed_impl = (reporter, expr_loc, err_loc) {
        assert(false, "Error: The list is not properly closed");
    }; 
    
    /**
     * Reports that the described token cannot start an expression
     */
    ReportingFunction_token token_cannot_start_expr_impl = (reporter, token) {
        assert(false, "Error: The token (" ~ token.start[0 .. token.span] ~ ") cannot start an expression");
    };

    /**
     * Reports that the token is not an operator (cannot be in infix position).
     */
    ReportingFunction_loc_token token_is_not_an_operator_impl = (reporter, loc, token) {
        assert(false, "Error: The token (" ~ token.start[0 .. token.span] ~ ") is not an operator");
    };
}
