import std.stdio;

void main(string[] args) {
    import std.file: readText;

    // test();

	if (args.length == 3 && args[1] == "-f")
	   print_ast(args[2], readText(args[2]));
	else if (args.length == 3 && args[1] == "-i")
	   print_ast("console", args[2]);
    else assert(false);
}

void print_ast(string filename, string text) {
    import arc.stringtable: StringTable;
    import arc.syntax.parser: Parser, parse_statement;
    import arc.syntax.reporter: SyntaxReporter;
    import arc.output.ast_printer: AstPrinter;
    import arc.syntax.location: SourceMap;
    

    StringTable table;
    SourceMap sources;
    auto source = sources.put(filename, text);
    auto error = new SyntaxReporter(source);
    auto parser = Parser(source.span, &table, error);

    auto printer = new AstPrinter(source.span);
    while (!parser.empty) {
        auto s = parser.parse_statement();
        printer.print(s);
    }
    assert(parser.empty);
    writeln(printer.data);
    writeln("errors: ", parser.reporter.errors);
    printer.reset();
}

void test() {
    import arc.syntax.tests.long_parser: text, types;
    import arc.syntax.tests.parser: parse;
    import arc.syntax.parser: parse_module;
    import arc.syntax.flat_ast: flatten;
    
    auto result = parse!parse_module(cast(const(char)[]) text);
    auto flat_tree = flatten(result.count, result.tree);

    import std.range: zip;
    int i;
    foreach (pair; zip(flat_tree, types)) {
        i++;
        if (pair[0].type != pair[1]) {
            writeln(i);
            break;
        }
    }
}
