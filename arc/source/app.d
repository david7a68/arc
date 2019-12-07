import arc.compiler;

CompilerOptions options;

auto handle_mode(string option) {
    import std.uni: icmp;

    if (icmp(option, "immediate") == 0)
        options.execution_mode = ExecutionMode.Immediate;
    else if (icmp(option, "file") == 0)
        options.execution_mode = ExecutionMode.File;
    else
        assert(false);
}

void main(string[] args) {
    import std.getopt: getopt, defaultGetoptPrinter;

    auto help = getopt(args,
        "immediate", &handle_mode,
        "file", &handle_mode,
        "pass", &options.final_pass,
    );
    
    if (help.helpWanted) {
        defaultGetoptPrinter("Some information about the program.", help.options);
        return;
    }
    
    const is_file_mode = options.execution_mode == ExecutionMode.File;
    if ((is_file_mode && args.length > 1) || (!is_file_mode && args.length == 1)) {
        options.first_file = is_file_mode ? args[1] : "";

        auto ctx = CompilerContext(options);
        ctx.execute();
        return;
    }

    assert(false, "unexpected arguments");
}
