import std.stdio;

// import arc.data.source_map;
// import arc.output.ast_printer;
// import arc.reporter;
// import arc.syntax.parser;

import arc.compiler;

void do_cli(string[] args) {
	auto compiler = new Compiler();

	compiler.compile(args[1 .. $], CompileOptions());
}

void main(string[] args) {
	do_cli(args);
}
