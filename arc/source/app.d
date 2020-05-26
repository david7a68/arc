import std.stdio;

// import arc.data.source_map;
// import arc.output.ast_printer;
// import arc.reporter;
// import arc.syntax.parser;

import arc.data.ast2;
import arc.memory;

void main(string[] args) {
	// g();
	f();
	// auto map = new SourceMap();
	// // auto src = map.put("", "(a = b)");
	// auto src = map.put("", "a : !;");
	// auto rep = Reporter();
	// auto prs = new Parser(&rep);

	// // prs.reset(src.text);
	// prs.reset(src.text);
	// auto ast = parse_statement(prs);
	// // auto ast = prs.parse_text(src.text);

	// foreach (error; rep.errors)
	// 	writeln(error);

	// auto txt = print_ast(map, ast);
	// writeln(txt);
}

void f() {
	import arc.data.ast2;
	import arc.syntax.parser2;
	import arc.syntax.tests.parser2;
	
	with (AstNode.Kind) {
		check_types("1 + 1".parse!"expression", Add, Integer, Integer);
        check_types("a ^ 2".parse!"expression", Power, Name, Integer);
    }
}