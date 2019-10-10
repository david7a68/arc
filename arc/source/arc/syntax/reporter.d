module arc.syntax.reporter;

import arc.hash: Key;
import arc.syntax.location;

class SyntaxReporter {
    SyntaxError[] errors;

    void error(A...)(SyntaxError error, string format, A args) {
        import std.stdio: writefln;
        
        writefln(format, args);
        errors ~= error;
    }

    void warn(A...)(string format, A args) {
        import std.stdio: writefln;
        
        writefln(format, args);
    }

    bool has_error(SyntaxError error) {
        foreach (e; errors)
            if (e == error)
                return true;
        return false;
    }
}

enum SyntaxError {
    NoError,
    TokenNotAnExpression,
    SequenceMissingClosingDelimiter,
    DefineMissingTypeSpec,
}
