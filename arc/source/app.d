import std.stdio;
import arc.syntax.ast;
import arc.syntax.lexer;
import arc.syntax.parser;
import arc.syntax.syntax_reporter;
import arc.output.ast_printer;
import arc.syntax.location;

void main() {
    auto a = Span(10, 20);
    auto b = Span(5, 15);
    auto c = Span(15, 25);
    auto d = Span(5, 25);
    assert(a.merge(a) == Span(10, 20));
    assert(a.merge(b) == Span(5, 25));
    assert(a.merge(c) == Span(10, 30));
    assert(a.merge(d) == Span(5, 25));
    assert(b.merge(c) == Span(5, 35));
}
