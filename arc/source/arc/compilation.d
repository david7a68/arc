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
    import arc.data.source: SourceMap, Source, Span;
    import arc.reporting: Reporter;
    import arc.ast: AstNode;
    import arc.semantic.scope_tree: ScopeTree;

    SourceMap sources;
    StringTable strings;
    Reporter message_reporter;

    // For a compilation that manages its own sources and string table
    this() {
        sources = new SourceMap();
        strings = new StringTable();

        import arc.syntax.lexer: initialize_token_strings;
        initialize_token_strings(strings);
    }

    AstNode* parse(Source source) {
        import arc.syntax.lexer: Lexer;
        import arc.syntax.parser: ParseCtx, parse_module;

        auto lex = Lexer(source.text, strings, source.span.start);
        auto ctx = ParseCtx(lex, &message_reporter);
        auto result = parse_module(ctx);

        const had_error = report_results(source);
        if (had_error)
            return null;

        return result;
    }

    ScopeTree build_scope_tree(AstNode* syntax) {
        import arc.semantic.scope_tree: ScopeTree, ScopeTreeBuilder, collect_symbols;

        auto builder = ScopeTreeBuilder.initialize();
        collect_symbols(builder, syntax);

        return builder.scope_tree;
    }

    void resolve_local_symbols(ScopeTree* tree) {
        // import arc.semantic.scope_tree: resolve_symbols;

        // resolve_symbols(tree.symbols, tree.root_scope);
    }

    bool report_results(Source current_source) {
        import std.stdio: writefln;

        if (message_reporter.warnings.length > 0) {
            foreach (warning; message_reporter.warnings) {
                const coords = current_source.get_loc(warning.location);
                writefln(
                    "Warning:\n%s\nAt %s line %s column %s\n",
                    warning.message,
                    current_source.name,
                    coords.line,
                    coords.column
                );
            }

            message_reporter.warnings = [];
        }

        const had_error = message_reporter.errors.length > 0;
        if (had_error) {
            foreach (error; message_reporter.errors) {
                const coords = current_source.get_loc(error.location);
                writefln(
                    "Error:\n%s\nAt %s line %s column %s\n",
                    error.message,
                    current_source.name,
                    coords.line,
                    coords.column
                );
            }

            message_reporter.errors = [];
        }

        return had_error;
    }
}
