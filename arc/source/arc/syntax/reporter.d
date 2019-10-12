module arc.syntax.reporter;

import arc.hash: Key;
import arc.syntax.location;

class SyntaxReporter {
    Source source;
    SyntaxError[] errors;

    this(Source source) { this.source = source; }

    void error(A...)(SyntaxError error, CharPos loc, string format, A args) {
        import std.stdio: writeln, writefln;
        writeln("Error:");
        writefln(format, args);

        auto err_coords = source.get_loc(loc);
        writefln("At %s line %s column %s", source.name, err_coords.line, err_coords.column);
        writeln();
        errors ~= error;
    }

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
    TokenExpectMismatch,
    TokenNotAnExpression,
    SequenceMissingClosingDelimiter,
    DefineMissingTypeSpec,
}
