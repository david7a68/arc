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
            auto source = source_map.put(path, readText(path));
            auto syntax = parse(this, source);

            import std.stdio: writeln;
            writeln(syntax);

            import arc.output.ast_printer: print_ast;
            writeln(print_ast(source_map, syntax));
            foreach (error; reporter.errors) writeln(error);
        }
    }
}

AstNode*[] parse(Compiler compiler, Source source) {
    import arc.data.ast_memory: SequenceBuilder;
    import arc.syntax.parser: ParsingContext, parse_statement;

    auto parser = ParsingContext(&compiler.reporter, compiler.node_allocator);
    auto statements = SequenceBuilder(compiler.node_allocator);
    size_t num_errors;

    parser.begin(source.text);
    while (!parser.is_done) {
        auto statement = parse_statement(&parser);

        // if (statement.is_valid)
        statements.add(statement);
        if (!statement.is_valid) num_errors++;

        if (num_errors == 5) break;
        // else {
        //     parser.resynchronize();
        //     compiler.node_allocator.free(statement);
        // }
    }

    return statements.nodes; // TODO: Implement parsing import expressions
}
