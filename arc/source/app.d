import std.stdio;
// import arc.syntax.lexer;
// import arc.syntax.parser;
// import arc.syntax.syntax_reporter;
// import arc.output.ast_printer;
// import arc.syntax.location;

void main(string[] args) {
    import std.file: readText;

	if (args.length == 3 && args[1] == "-f")
	   print_ast(args[2], readText(args[2]));
	else if (args.length == 3 && args[1] == "-i")
	   print_ast("console", args[2]);
	// print_ast("", readText("sample.arc"));
}

void print_ast(string filename, string text) {
    import arc.stringtable: StringTable;
    import arc.syntax.parser: Parser, statement;
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
        auto s = parser.statement();
        printer.print(s);
    }
    assert(parser.empty);
    writeln(printer.data);
    writeln("errors: ", parser.reporter.errors);
    printer.reset();
}
