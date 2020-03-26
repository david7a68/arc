module arc.compiler;

enum ExecutionMode: ubyte {
    File,
    Immediate,
}

enum CompilerPass: ubyte {
    all,
    parse,
    scopes,
}

struct CompilerOptions {
    ExecutionMode execution_mode;
    CompilerPass[] passes_to_print;
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
        import std.algorithm: canFind;
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

        if (options.passes_to_print.canFind(CompilerPass.parse))
            dump_ast(compilation, syntax);

        auto scopes = compilation.build_scope_tree(syntax);

        if (options.passes_to_print.canFind(CompilerPass.scopes))
            dump_scope_tree(compilation, scopes);
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

    import arc.semantic.scope_tree: ScopeTree;
    void dump_scope_tree(Compilation compilation, ScopeTree tree) {
        import std.stdio: writeln, writefln;
        import arc.semantic.symbol: Symbol;

        writeln("======== Symbols ========");
        foreach (symbol; tree.symbols) {
            // if (symbol.kind == Symbol.Reference)
            //     writefln("%s %s %s", symbol.kind, compilation.strings.lookup(symbol.name), symbol.referred_symbol);
            // else
                writefln("%s %s", symbol.kind, compilation.strings.lookup(symbol.name));
        }
        writeln();
    }
}
