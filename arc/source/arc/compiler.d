module arc.compiler;

enum ExecutionMode: ubyte {
    File,
    Immediate,
}

enum TerminalCompilerPass: ubyte {
    all,
    parse,
}

struct CompilerOptions {
    ExecutionMode execution_mode;
    TerminalCompilerPass final_pass;
    const(char)[] first_file;
}

/**
 * A compiler context is the central repository for all data that must be
 * stored for complete source analysis and translation.
 *
 * To clean up all the memory of a compilation, just destroy the context.
 */
struct CompilerContext {
    import arc.stringtable: StringTable;
    import arc.source: SourceMap, Source;

    StringTable strings;
    SourceMap sources;
    CompilerOptions options;

    this(CompilerOptions options) {
        this.options = options;
        sources = new SourceMap();
        strings = new StringTable();
    }

    void execute() {
        Source first_source;
        switch (options.execution_mode) with (ExecutionMode) {
            case Immediate:
                import std.stdio: readln, write;
                write(":>> ");
                first_source = sources.put("command line", readln());
                break;
            case File:
                first_source = sources.put(options.first_file, load_file(options.first_file));
                break;
            default:
        }

        compile(first_source);
    }

    void compile(Source source) {
        auto syntax = parse(source);

        if (syntax is null)
            return;

        if (options.final_pass == TerminalCompilerPass.parse) {
            dump_ast(syntax);
            return;
        }
    }

    const(char)[] load_file(const(char)[] filename) {
        import std.file: readText;

        return readText(filename);
    }

    import arc.syntax.ast: AstNode;
    AstNode parse(Source source) {
        import std.stdio: writefln;
        import arc.syntax.parser: ParseCtx, parse_module;

        auto ctx = ParseCtx(source.text, source.start_offset, strings);
        auto result = parse_module(ctx);

        if (ctx.warnings.length > 0) {
            foreach (warning; ctx.warnings) {
                const coords = source.get_loc(warning.location);
                writefln(
                    "Warning:\n%s\nAt %s line %s column %s\n",
                    warning.message,
                    source.name,
                    coords.line,
                    coords.column
                );
            }
        }

        if (ctx.errors.length > 0) {
            foreach (error; ctx.errors) {
                const coords = source.get_loc(error.location);
                writefln(
                    "Error:\n%s\nAt %s line %s column %s\n",
                    error.message,
                    source.name,
                    coords.line,
                    coords.column
                );
            }

            return null;
        }

        return result;
    }

    void dump_ast(AstNode root) {
        import std.stdio: writeln;
        import arc.output.ast_printer: AstPrinter;

        auto printer = new AstPrinter(sources);
        printer.print(root);
        writeln(printer.data);
    }
}
