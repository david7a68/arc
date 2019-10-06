import std.stdio;
import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.parser;
import arc.syntax.syntax_reporter;
import arc.output.ast_printer;
import arc.syntax.location;

void main(string[] args) {
    if (args.length == 3 && args[1] == "-f")
        print_ast(args[2]);
}

void print_ast(string filename) {
    writeln("print ast");
    import std.file: readText;
    import arc.stringtable: StringTable;
    import arc.syntax.parser: Parser;
    import arc.syntax.syntax_reporter: SyntaxReporter;
    import arc.output.ast_printer: AstPrinter;
    import arc.syntax.location: SourceMap;
    


    StringTable table;
    SourceMap sources;
    auto source = sources.put(filename, readText(filename));
    auto error = SyntaxReporter(null, source);
    auto parser = Parser(source.span, &table, &error);


    
    auto printer = new AstPrinter(source.span);
    while (!parser.empty) {
        auto s = parser.statement();
        s.accept(printer);
    }
    writeln(printer.data);
    printer.reset();
}
