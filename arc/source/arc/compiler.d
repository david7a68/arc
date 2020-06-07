module arc.compiler;

struct CompileOptions {
    string output_path; // CLEANUP: Maybe turn this into a path object or smth?
    string output_name;
}

import arc.data.ast;
import arc.data.ast_memory: AstNodeAllocator;
import arc.data.source: Source;
import arc.data.source_map: SourceMap;
import arc.reporter: Reporter;

final class Compiler {
    AstNodeAllocator node_allocator;
    SourceMap source_map;
    Reporter reporter;

    this() {
        node_allocator = new AstNodeAllocator();
        source_map = new SourceMap();
    }

    void compile(string[] source_paths, CompileOptions options) {
        import std.file: readText;

        foreach (path; source_paths) {
            auto syntax = parse(path, readText(path));
            print_ast(syntax);
        }
    }

    AstNode*[] parse(string name, string source) {
        import arc.data.ast_memory: SequenceBuilder;
        import arc.syntax.parser: ParsingContext, parse_statement;

        source_map.put(name, source);
        
        auto statements = SequenceBuilder(node_allocator);
        auto parser = ParsingContext(&reporter, node_allocator);
        parser.begin(source);

        size_t num_errors;
        while (!parser.is_done) {
            auto statement = parse_statement(&parser);

            statements.add(statement);
            if (!statement.is_valid) num_errors++;

            if (num_errors == 5) break;
        }

        return statements.nodes; // TODO: Implement parsing import expressions
    }

    void print_ast(AstNode*[] nodes) {
        import arc.output.ast_printer: print_ast;
        import std.stdio: writeln;
        
        writeln(print_ast(source_map, nodes));
        report_errors(&reporter, source_map);
    }
}

void report_errors(Reporter* reporter, SourceMap sources) {
    import std.stdio: writefln;
    import arc.data.source: Span;

    foreach (error; reporter.errors) {
        auto coords = sources.coordinates_of(Span(error.location, 0));

        writefln("Error: %s\n\tAt (line: %s, column: %s) in file %s",
            error.message,
            coords.line,
            coords.column,
            sources.source_of(Span(error.location, 0)).path,
        );
    }
}