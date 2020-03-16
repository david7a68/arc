module arc.reporting;

struct ArcError {
    enum Code {
        NoError,
        UnexpectedEndOfFile,
        TokenExpectMismatch,
        TokenNotAnExpression,
        UnboundElse,
    }

    alias Code this;

    Code code;
    // Source parsed_file;
    uint location;
    string message;
}

struct ArcWarning {
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
