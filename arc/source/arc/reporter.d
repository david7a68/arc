module arc.reporter;

struct ArcError {
    enum Code {
        NoError,
        DebugError,
        UnexpectedEOF,
        TokenExpectMismatch,
        TokenNotAnExpression,
        UnboundElse,
        IncompleteDefine
    }

    alias Code this;

    Code code;
    // Source parsed_file;
    size_t location;
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
    size_t location;
    string message;
}

struct Reporter {
    import arc.data.span : Span;

    ArcError[] errors;
    ArcWarning[] warnings;

    void error(Args...)(ArcError.Code error_code, Span span, const char[] message, Args args) {
        error(error_code, span, tprint(message, args));
    }

    void error(ArcError.Code error_code, Span span, const char[] message) {
        errors ~= ArcError(error_code, span.start, message.idup);
    }

    void warn(Args...)(ArcWarning.Code warn_code, Span span, const char[] message, Args args) {
        warn(warn_code, span, tprint(message, args));
    }

    void warn(ArcWarning.Code warn_code, Span span, const char[] message) {
        warnings ~= ArcWarning(warn_code, span.start, message.idup);
    }

    void clear() {
        errors.length = 0;
        warnings.length = 0;
    }

    bool has_error(ArcError.Code ecode) {
        foreach (error; errors)
            if (error.code == ecode)
                return true;

        return false;
    }
}

const(char[]) tprint(Args...)(const char[] message, Args args) {
    import std.format : formattedWrite;

    static struct Buffer {
        char[] data;
        size_t length;

        void put(char c) {
            assert(length < data.length);
            data[length] = c;
            length++;
        }

        const(char[]) text() const {
            return data[0 .. length];
        }
    }

    static char[4096] temp_buffer;

    auto buffer = Buffer(temp_buffer);
    formattedWrite(buffer, message, args);

    return buffer.text();
}
