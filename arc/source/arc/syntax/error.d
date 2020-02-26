module arc.syntax.error;

struct SyntaxError {
    import arc.source: Source;

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
