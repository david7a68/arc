module arc.compiler;

import arc.data.ast;
import arc.data.symbol;
import arc.memory;
import arc.reporter : Reporter;
import arc.analysis.source_analyzer;
import arc.source;
import arc.source_map;

struct CompileOptions {
    string first_file;
    string output_path; // CLEANUP: Maybe turn this into a path object or smth?
    string output_name;
}

final class Compiler {

    enum initial_source_map_slots = 64;

public:
    Reporter reporter;
    SourceMap sources;
    SourceAnalyzer source_analyzer;

    this() {
        source_analyzer = SourceAnalyzer(&reporter);
        sources = SourceMap(initial_source_map_slots);
    }

    void compile(CompileOptions options) {
        import std.file : readText;

        auto src = sources.put(options.first_file, readText(options.first_file));
        source_analyzer.add_source(src);
        auto ast = source_analyzer.ast_of(src);

        print_ast(ast);
    }

    void print_ast(SyntaxTree ast) {
        import arc.output.ast_printer : print_ast;
        import std.stdio : writeln;

        writeln(print_ast(&source_analyzer, ast));
        report_errors(&reporter, ast.source);
    }
}

void report_errors(Reporter* reporter, Source* source) {
    import std.stdio : writefln;
    import arc.data.span : Span;

    foreach (error; reporter.errors) {
        auto coords = source.coords_of(error.location);

        writefln("Error: %s\n\tAt (line: %s, column: %s) in file %s",
            error.message, coords.line, coords.column, source.path);
    }
}
