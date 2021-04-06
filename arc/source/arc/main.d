module arc.main;

import std.file : exists, read_text = readText;
import std.getopt : getopt;
import shard.memory.mem_api;
import shard.memory.tracker : MemoryStats;
import shard.os.api;
import shard.logger;
import arc.log_out;
import arc.unit : CompilationUnit;

struct CompilerOptions {
    bool colorize = true;
    bool memory_stats = false;
    string[] extra_args;
}

void main(string[] args) {
    CompilerOptions options;
    // dfmt off
    const result = getopt(
        args,
        "colorize",
            "Toggles colorized console output. Defaults to true.",
            &options.colorize,
        "memory-stats",
            "Toggles output of memory usage statistics after compilation completes. Defaults to true.",
            &options.memory_stats,
    );
    // dfmt on

    if (result.helpWanted) {

    }

    options.extra_args = args[1 .. $];
    run(options);
}

void run(const ref CompilerOptions options) {
    auto memory = MemoryApi(default_temp_size);
    scope clock = new OsClock();

    auto logger = Logger(LogLevel.All, clock);
    scope sink = new CompilerOut(options.colorize);

    const sink_id = logger.add_sink(sink);
    scope (exit) logger.remove_sink(sink_id);

    logger.all("Arc Compiler v0.0.0");

    if (options.extra_args.length == 1) {
        const file_name = options.extra_args[0];
        if (file_name.length <= 4 || file_name[$ - 4 .. $] != ".arc")
            logger.fatal("The source file argument must end with the '.arc' extension.");
        else if (!exists(file_name))
            logger.fatal("The source file \"%s\" could not be found.", file_name);
        else {
            logger.all("Source file found, compiling would go here.");
            auto cu = CompilationUnit(file_name);
            // work_queue.add_compilation_unit(cu);
            // while (work_queue.has_work)
            //     work_queue.execute();
        }
    }
    else if (options.extra_args.length == 0)
        logger.fatal("At least one source file is needed for useful work to be done. Simply add it to the command line as something like the following:\n    `arc <options> main.arc`");
    else
        logger.fatal("Only one source file is needed for the compiler to build a project. All dependent files in the same project will be found automatically. If you are attempting to include a separate project as a dependency, such functionality is not yet supported.");

    MemoryStats sys_stats;
    memory.get_sys_stats(sys_stats);

    if (options.memory_stats) {
        logger.info(
"Memory Statistics:
    Temp Memory Region Size: %s
    Max Non-Temp Memory Used: %s
    # System Allocations: %s
    # Leaked System Allocations",
            memory.temp_region_size,
            sys_stats.most_bytes_allocated,
            sys_stats.total_allocations,
            sys_stats.num_allocations);
    }
}
