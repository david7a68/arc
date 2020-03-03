module arc.syntax.reporting;

struct SyntaxError {
    enum Code {
        NoError,
        UnexpectedEndOfFile,
        TokenExpectMismatch,
        TokenNotAnExpression,
    }

    alias Code this;

    Code code;
    // Source parsed_file;
    uint location;
    string message;
}

struct SyntaxWarning {
    enum Code {
        /// A semicolon-only statement should be replaced with an empty block ({})
        LonelySemicolon,
        TooManyErrors,
    }

    alias Code this;

    Code code;
    uint location;
    string message;
}
