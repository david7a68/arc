import std.stdio;
import arc.source;
import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.parser;
import arc.syntax.syntax_reporter;
import arc.output.ast_printer;


void main() {
    auto p = new AstPrinter;
    
    Parser parser;
    auto err = SyntaxReporter();
    parser.reset("(a, b) -> [
        a
        a * b
        b
    ]");
    auto e = parser.expression(err);
    assert(parser.token.type == Token.Eof);
    e.accept(p);
    writeln(p.data);
}
