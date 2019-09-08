module arc.syntax.syntax_reporter;

struct SyntaxReporter {
    import arc.source: StringID;
    import arc.syntax.lexer: Token;

    StringID source_id;
    void* user_data;

    /// Function type that reports the location of the error, and the location of its enclosing expression
    alias ReportingFunction_loc_err = void function(SyntaxReporter*, const(char)* expr, const(char)* err);
    /// Function type that reports the location of the error
    alias ReportingFunction_loc = void function(SyntaxReporter*, const(char)* loc);
    /// Function type that reports a particular token
    alias ReportingFunction_token = void function(SyntaxReporter*, Token token);

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
     * Reports that the tuple starting at 'loc' is missing a comma at 'err_loc'
     */
    ReportingFunction_loc_err tuple_missing_comma_impl = (reporter, expr_loc, err_loc) {
        assert(false, "Error: Missing comma in tuple expression");
    };

    /**
     * Reports that the vector starting at 'loc' is missing a comma at 'err_loc'
     */
    ReportingFunction_loc_err vector_missing_comma_impl = (reporter, expr_loc, err_loc) {
        assert(false, "Error: Missing comma in vector expression");
    };

    /**
     * Reports that the described token cannot start an expression
     */
    ReportingFunction_token token_cannot_start_expr_impl = (reporter, token) {
        assert(false, "Error: The token (" ~ token.start[0 .. token.span] ~ ") cannot start an expression");
    };

    /**
     * Reports the unexpected end of the file while parsing the syntactical
     * artifact starting at 'loc'
     */
    ReportingFunction_loc unexpected_end_of_file_impl = (reporter, loc) {
        assert(false, "Error: Unexpected end of file!");
    };
}
