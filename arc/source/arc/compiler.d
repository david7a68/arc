module arc.compiler;

struct CompileOptions {
    string output_path; // CLEANUP: Maybe turn this into a path object or smth?
    string output_name;
}

import arc.data.ast;
import arc.data.symbol;
import arc.memory;
import arc.reporter : Reporter;
import arc.source : Source;
import arc.source_map : SourceMap;
import arc.data.stringtable: StringTable;

struct Executor {
    import arc.analysis.parser : Parser, ParseUnit;

public:
    Parser parser;
    VirtualMemory ast_memory;
    ArrayPool!(AstNode*) ast_arrays;

    Reporter* global_error_reporter;
    SymbolTable* global_symbol_table;
    SourceMap global_source_map;
    StringTable strings;

    this(size_t ast_memory_size, Reporter* reporter, SymbolTable* symtab, SourceMap sources) {
        ast_memory = VirtualMemory(ast_memory_size);
        ast_arrays = typeof(ast_arrays)(&ast_memory);

        global_error_reporter = reporter;
        global_symbol_table = symtab;
        global_source_map = sources;

        strings = StringTable(128);
    }

    AstNode*[] parse(Source source) {
        auto unit = ParseUnit(&ast_memory, &ast_arrays, global_error_reporter, global_symbol_table, &strings, source.text);
        return parser.parse(unit);
    }
}

final class Compiler {
    Executor executor;

    Reporter reporter;
    SourceMap source_map;

    VirtualMemory global_vm;
    SymbolTable symbol_table;

    this() {
        source_map = new SourceMap();
        global_vm = VirtualMemory(128.gib);
        symbol_table = SymbolTable(&global_vm);
        executor = Executor(128.gib, &reporter, &symbol_table, source_map);
    }

    void compile(string[] source_paths, CompileOptions options) {
        import std.file : readText;

        foreach (path; source_paths) {
            auto syntax = parse(path, readText(path));
            print_ast(syntax);
        }
    }

    AstNode*[] parse(string name, string source_text) {
        auto source = source_map.put(name, source_text);
        return executor.parse(source);
    }

    void print_ast(AstNode*[] nodes) {
        import arc.output.ast_printer : print_ast;
        import std.stdio : writeln;

        writeln(print_ast(&executor.strings, nodes));
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
