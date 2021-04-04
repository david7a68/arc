import std.stdio;

import arc.compiler;

int do_cli(string[] args) {
	auto compiler = new Compiler();

	compiler.compile(CompileOptions(args[1]));

	if (compiler.reporter.errors.length > 0)
		return -compiler.reporter.errors[0].code;
	return 0;
}

int main(string[] args) {
	return do_cli(args);
}
