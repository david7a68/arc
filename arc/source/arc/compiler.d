module arc.compiler;

struct CompileOptions {
    string output_path; // CLEANUP: Maybe turn this into a path object or smth?
    string output_name;
}

import arc.data.ast;
import arc.data.symbol;
import arc.data.source : Source;
import arc.data.source_map : SourceMap;
import arc.reporter : Reporter;
import arc.memory;

final class Compiler {
    VirtualMemory ast_memory;
    ArrayPool!(AstNode*) ast_arrays;

    SymbolTable symbol_table;

    SourceMap source_map;
    Reporter reporter;

    this() {
        ast_memory = VirtualMemory(128.gib);
        ast_arrays = ArrayPool!(AstNode*)(&ast_memory);
        symbol_table = SymbolTable(&ast_memory);

        source_map = new SourceMap();
    }

    void compile(string[] source_paths, CompileOptions options) {
        import std.file : readText;

        foreach (path; source_paths) {
            auto syntax = parse(path, readText(path));
            print_ast(syntax);
        }
    }

    AstNode*[] parse(string name, string source) {
        import arc.syntax.parser : Parser, ParseUnit;

        source_map.put(name, source);

        auto statements = ast_arrays.get_appender();
        Parser parser;
        parser.begin(ParseUnit(&ast_memory, &ast_arrays, &reporter, &symbol_table, source));

        size_t num_errors;
        while (!parser.is_done) {
            auto statement = parser.stmt();

            statements ~= statement;
            if (!statement.is_valid)
                num_errors++;

            if (num_errors == 5)
                break;
        }

        return statements.get(); // TODO: Implement parsing import expressions
    }

    void print_ast(AstNode*[] nodes) {
        import arc.output.ast_printer : print_ast;
        import std.stdio : writeln;

        writeln(print_ast(source_map, nodes));
        report_errors(&reporter, source_map);
    }
}

void report_errors(Reporter* reporter, SourceMap sources) {
    import std.stdio : writefln;
    import arc.data.span : Span;

    foreach (error; reporter.errors) {
        auto coords = sources.coordinates_of(Span(error.location, 0));

        writefln("Error: %s\n\tAt (line: %s, column: %s) in file %s", error.message,
                coords.line, coords.column, sources.source_of(Span(error.location, 0)).path,);
    }
}
