import arc.compiler;

CompilerOptions options;

auto handle_mode(string option) {
    import std.uni: icmp;

    if (icmp(option, "i|immediate") == 0)
        options.execution_mode = ExecutionMode.Immediate;
    else if (icmp(option, "f|file") == 0)
        options.execution_mode = ExecutionMode.File;
    else
        assert(false, option);
}

void do_cli(string[] args) {
    import std.getopt: getopt, defaultGetoptPrinter;

    auto help = getopt(args,
        "i|immediate", &handle_mode,
        "f|file", &handle_mode,
        "p|pass", &options.final_pass,
    );
    
    if (help.helpWanted) {
        defaultGetoptPrinter("Some information about the program.", help.options);
        return;
    }

    const is_file_mode = options.execution_mode == ExecutionMode.File;
    if ((is_file_mode && args.length > 1) || (!is_file_mode && args.length == 1)) {
        options.first_file = is_file_mode ? args[1] : "";

        auto ctx = Compiler();
        ctx.execute(options);
        return;
    }

    assert(false, "unexpected arguments");
}

void main(string[] args) {
    do_cli(args);
}
