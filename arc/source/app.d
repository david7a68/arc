import std.stdio;

// import arc.data.source_map;
// import arc.output.ast_printer;
// import arc.reporter;
// import arc.syntax.parser;

import arc.data.ast2;
import arc.memory;

void main(string[] args) {
	auto nodes = AstNodeAllocator.initialize();

    auto list = AstNodeList(&nodes, nodes.alloc());
    for (int i = 0; i < 31; i++) // for 32 nodes total
        list.add(nodes.alloc());

    assert(nodes.num_allocated == 32);

	nodes.free(list.head);

    assert(nodes.num_allocated == 0);
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
