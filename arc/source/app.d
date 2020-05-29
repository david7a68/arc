import std.stdio;

// import arc.data.source_map;
// import arc.output.ast_printer;
// import arc.reporter;
// import arc.syntax.parser;

import arc.data.ast;
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
	import arc.data.ast;
	import arc.syntax.parser;
	import arc.syntax.tests.parser;
	import arc.reporter;
	import arc.data.source;

	auto error = "def T : (!);".parse!"statement"();
	error.span.writeln;
	check_error(error, ArcError.TokenExpectMismatch, 1);
	writeln(reporter.errors);
}

void g() {
	import arc.syntax.tests.lexer;
	import arc.syntax.lexer;

	assert("#blah blah blah".scan_tokens[0].type == Token.Done);
    assert("#blah\n#blah".scan_tokens[0].type == Token.Done);
}
