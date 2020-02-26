module arc.compiler;

enum ExecutionMode: ubyte {
    File,
    Immediate,
}

enum TerminalCompilerPass: ubyte {
    all,
    parse,
    build_symbols,
    check_symbols,
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

        // if (options.final_pass == TerminalCompilerPass.parse) {
        //     dump_ast(source, syntax);
        //     return;
        // }


        // auto symbols = build_symbol_tree(syntax);

        // if (options.final_pass == TerminalCompilerPass.build_symbols) {
        //     dump_symbols(symbols);
        //     return;
        // }

        // if (options.final_pass == TerminalCompilerPass.all) {
        //     dump_symbols(symbols);
        //     return;
        // }
    }

    Source load_source(const(char)[] filename) {
        import std.file: readText;

        const(char)[] text = readText(filename);
        return sources.put(filename, text);
    }

    import arc.syntax.ast: AstNode;
    AstNode parse(Source source) {
        import std.stdio: writeln;
        import arc.syntax.parser: ParseCtx, parse_module;

        auto ctx = ParseCtx(source.text, source.start_offset);
        auto result = parse_module(ctx);

        if (ctx.errors.length > 0) {
            foreach (error; ctx.errors) {
                writeln("Error:\n", error.message);

                const coords = source.get_loc(error.location);
                writeln("At %s line %s column %s\n", source.name, coords.line, coords.column);
            }
        }

        return result;
    }

    // void dump_ast(Source source, AstNode root) {
    //     import std.stdio: writeln;
    //     import arc.output.ast_printer: AstPrinter;

    //     auto printer = new AstPrinter(source);
    //     printer.print(root);
    //     writeln(printer.data);
    // }

    // import arc.semantic.symbol: Symbol;
    // Symbol* build_symbol_tree(AstNode syntax) {
    //     import arc.semantic.scope_builder: ScopeBuilder, build_symbol_tree;

    //     auto builder = ScopeBuilder();
    //     builder.module_scope = builder.current_scope = new Symbol(Symbol.Scope);
    //     build_symbol_tree(builder, syntax);
    //     return builder.module_scope;
    // }

    // void dump_symbols(Symbol* symbol_tree) {
    //     import std.stdio: writeln;
    //     import arc.output.symbol_printer: SymbolPrinter;

    //     auto printer = new SymbolPrinter(&strings);
    //     printer.print(symbol_tree);
    //     writeln(printer.data);
    // }
}
