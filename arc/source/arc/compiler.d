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
struct Compiler {
    import arc.compilation: Compilation;

    void execute(CompilerOptions options) {
        import arc.source: Source;

        auto compilation = new Compilation();

        Source first_source;
        switch (options.execution_mode) with (ExecutionMode) {
            case Immediate:
                import std.stdio: readln, write;
                write(":>> ");
                first_source = compilation.sources.put("command line", readln());
                break;
            case File:
                first_source = compilation.sources.put(options.first_file, load_file(options.first_file));
                break;
            default:
        }

        auto syntax = compilation.parse(first_source);

        if (options.final_pass == TerminalCompilerPass.parse) {
            dump_ast(compilation, syntax);
        }
    }

    const(char)[] load_file(const(char)[] filename) {
        import std.file: readText;

        return readText(filename);
    }

    import arc.syntax.ast: AstNode;
    void dump_ast(Compilation compilation, AstNode root) {
        import std.stdio: writeln;
        import arc.output.ast_printer: AstPrinter;

        auto printer = new AstPrinter(compilation.sources);
        printer.print(root);
        writeln(printer.data);
    }
}
