module arc.reporter;

struct ArcError {
    enum Code {
        NoError,
        UnexpectedEndOfFile,
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
    import arc.data.source: Span;

    ArcError[] errors;
    ArcWarning[] warnings;

    void error(Args...)(ArcError.Code error_code, Span span, string message, Args args) {
        errors ~= ArcError(
            error_code,
            span.start,
            tprint(message, args).idup
        );
    }

    void warn(Args...)(ArcWarning.Code warn_code, Span span, string message, Args args) {
        warnings ~= ArcWarning(
            warn_code,
            span.start,
            tprint(message, args).idup
        );
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

    const(char[]) tprint(Args...)(string message, Args args) {
        import std.format: formattedWrite;

        static struct Buffer {
            char[] data;
            size_t length;

            void put(char c) {
                assert(length < data.length);
                data[length] = c;
                length++;
            }

            const(char[]) text() const { return data[0 .. length]; }
        }

        static char[4096] temp_buffer;

        auto buffer = Buffer(temp_buffer);
        formattedWrite(buffer, message, args);
        
        return buffer.text();
    }
}
