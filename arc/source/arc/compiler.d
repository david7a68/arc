module arc.compiler;

struct CompileOptions {
    string output_path; // CLEANUP: Maybe turn this into a path object or smth?
    string output_name;
}

import arc.data.ast;
import arc.data.symbol;
import arc.memory;
import arc.reporter : Reporter;
import arc.analysis: SourceAnalyzer, SourceUnit;

final class Compiler {
    Reporter reporter;
    SourceAnalyzer source_analyzer;

    this() {
        source_analyzer = SourceAnalyzer(&reporter);
    }

    void compile(string[] source_paths, CompileOptions options) {
        import std.file : readText;

        foreach (path; source_paths) {
            source_analyzer.update_file(path, readText(path));
            print_ast(source_analyzer.get_file(path));
        }
    }

    void print_ast(SourceUnit* source) {
        import arc.output.ast_printer : print_ast;
        import std.stdio : writeln;

        writeln(print_ast(source_analyzer.stringtable, source.syntax_tree));
        report_errors(&reporter, source);
    }
}

void report_errors(Reporter* reporter, SourceUnit* source) {
    import std.stdio : writefln;
    import arc.data.span : Span;

    foreach (error; reporter.errors) {
        auto coords = source.coords_of(error.location);

        writefln("Error: %s\n\tAt (line: %s, column: %s) in file %s",
            error.message, coords.line, coords.column, source.path);
    }
}
