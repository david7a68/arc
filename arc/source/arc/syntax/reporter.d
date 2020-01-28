module arc.syntax.reporter;

import arc.hash: Key;
import arc.syntax.location;

struct SyntaxReporter {
    Source source;
    bool silent;
    SyntaxError[] errors;

    void error(A...)(SyntaxError error, CharPos loc, string format, A args) {
        import std.stdio: writeln, writefln;
        
        errors ~= error;

        if (!silent) {
            writeln("Error:");
            writefln(format, args);

            auto err_coords = source.get_loc(loc);
            writefln("At %s line %s column %s\n", source.name, err_coords.line, err_coords.column);
        }
    }

    void error(A...)(SyntaxError error, string format, A args) {
        import std.stdio: writefln;
        
        errors ~= error;

        if (!silent) {
            writefln(format, args);
        }
    }

    void warn(A...)(string format, A args) {
        import std.stdio: writefln;
        
        if (!silent) {
            writefln(format, args);
        }
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
    UnexpectedEndOfFile,
    TokenExpectMismatch,
    TokenNotAnExpression,
}
