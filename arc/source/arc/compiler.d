module arc.compiler;

enum ExecutionMode {
    File,
    Immediate,
}

struct CompilerOptions {
    ExecutionMode execution_mode;
    bool dump_ast;
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
    import arc.syntax.location: SourceMap, Source;
    import arc.syntax.reporter: SyntaxReporter;

    StringTable strings;
    SourceMap sources;
    SyntaxReporter syntax_errors;
    CompilerOptions options;

    this(CompilerOptions options) {
        this.options = options;
        syntax_errors = new SyntaxReporter();
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
                first_source = load_source(options.first_file);
                break;
            default:
        }

        compile(first_source);
    }

    void compile(Source source) {
        auto syntax = parse(source);

        if (options.dump_ast) {
            import std.stdio: writeln;
            import arc.output.ast_printer: AstPrinter;
            auto printer = new AstPrinter(source.span);
            printer.print(syntax);
            writeln(printer.data);
        }
    }

    import arc.syntax.ast: AstNode;
    AstNode* parse(Source source) {
        import arc.syntax.parser: Parser, parse_module;

        syntax_errors.reset(source);
        auto parser = Parser(source.span, &strings, syntax_errors);
        return parser.parse_module();
    }

    Source load_source(const(char)[] filename) {
        import std.file: readText;

        const(char)[] text = readText(filename);
        return sources.put(filename, text);
    }
}
