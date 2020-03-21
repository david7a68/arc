/**
 * Data required for compilation 
 */
module arc.compilation;

/**
 * A shared structure for data that is required at every phase of the 
 * compilation process. It is designed to be as small as possible, so that it
 * may be passed between each stage of compilation.
 *
 * This includes things like access to the other files in the compilation,
 * error handling faciliites, and memory management.
 *
 * Notably, this excludes anything to do with threading, file loading, and other
 * such facilities.
 */
final class Compilation {
    import arc.stringtable: StringTable;
    import arc.source: SourceMap, Source, Span;
    import arc.reporting: ArcError, ArcWarning;
    import arc.syntax.ast: AstNode;

    SourceMap sources;
    StringTable strings;
    ArcError[] errors;
    ArcWarning[] warnings;

    // For a compilation that manages its own sources and string table
    this() {
        sources = new SourceMap();
        strings = new StringTable();
    }

    void error(Args...)(ArcError.Code error_code, Span span, string message, Args args) {
        errors ~= ArcError(
            error_code,
            span.start,
            tprint(message, args).idup
        );
    }

    void warning(Args...)(ArcWarning.Code warn_code, Span span, string message, Args args) {
        warnings ~= ArcWarning(
            warn_code,
            span.start,
            tprint(message, args).idup
        );
    }

    AstNode parse(Source source) {
        import arc.syntax.parser: ParseCtx, parse_module;

        auto ctx = ParseCtx(this, source.text, source.start_offset);
        auto result = parse_module(ctx);

        const had_error = report_results(source);
        if (had_error)
            return null;

        build_scope_tree(result);

        return result;
    }

    void build_scope_tree(AstNode syntax) {
        import arc.semantic.scope_tree: ScopeTree, ScopeTreeBuilder, collect_declarations;

        auto builder = ScopeTreeBuilder.init();
        collect_declarations(builder, syntax);

        auto tree = builder.scope_tree;

        import std.stdio: writeln, writefln;
        writeln("======== Symbols ========");
        foreach (symbol; tree.symbols.byValue()) {
            writefln("%s %s", symbol.kind, strings.lookup(symbol.name));
        }
        writeln();
    }

    bool report_results(Source current_source) {
        import std.stdio: writefln;

        if (warnings.length > 0) {
            foreach (warning; warnings) {
                const coords = current_source.get_loc(warning.location);
                writefln(
                    "Warning:\n%s\nAt %s line %s column %s\n",
                    warning.message,
                    current_source.name,
                    coords.line,
                    coords.column
                );
            }

            warnings = [];
        }

        const had_error = errors.length > 0;
        if (had_error) {
            foreach (error; errors) {
                const coords = current_source.get_loc(error.location);
                writefln(
                    "Error:\n%s\nAt %s line %s column %s\n",
                    error.message,
                    current_source.name,
                    coords.line,
                    coords.column
                );
            }

            errors = [];
        }

        return had_error;
    }
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
