module arc.syntax.error;

struct SyntaxError {
    import arc.syntax.location: Source, CharPos;

    enum Code {
        NoError,
        UnexpectedEndOfFile,
        TokenExpectMismatch,
        TokenNotAnExpression,
    }

    alias Code this;

    Code code;
    Source parsed_file;
    CharPos location;
    string message;
}
