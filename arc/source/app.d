import std.stdio;

import arc.data.source_map;
import arc.output.ast_printer;
import arc.reporter;
import arc.syntax.parser;

void main(string[] args) {
	auto map = new SourceMap();
	// auto src = map.put("", "(a = b)");
	auto src = map.put("", "( { ) }");
	auto rep = Reporter();
	auto prs = new Parser(&rep);

	// prs.reset(src.text);
	auto ast = prs.parse_text(src.text);

	foreach (error; rep.errors)
		writeln(error);

	auto txt = print_ast(map, ast);
	writeln(txt);
}
