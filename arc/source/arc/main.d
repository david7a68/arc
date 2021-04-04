module arc.main;

import std.getopt : getopt;
import shard.memory.mem_api;
import shard.memory.tracker : MemoryStats;
import shard.os.api;
import shard.logger;
import arc.log_out;

struct CompilerOptions {
    bool colorize = true;
    bool memory_stats = false;
    string[] files;
}

void main(string[] args) {
    CompilerOptions options;
    const help = getopt(
        args,
        "colorize", "Toggles colorized console output. Defaults to true.", &options.colorize,
        "memory-stats", "Toggles output of memory usage statistics after compilation completes. Defaults to true.", &options.memory_stats,
    );

    if (help.helpWanted) {

    }

    options.files = args[1 .. $];
    run(options);
}

void run(const ref CompilerOptions options) {
    auto memory = MemoryApi(default_temp_size);
    scope clock = new OsClock();

    auto logger = Logger(LogLevel.All, clock);
    scope sink = new CompilerOut(options.colorize);

    const sink_id = logger.add_sink(sink);
    scope (exit) logger.remove_sink(sink_id);

    logger.info("Arc Compiler v0.0.0");

    MemoryStats sys_stats;
    memory.get_sys_stats(sys_stats);

    if (options.memory_stats) {
        logger.info("Memory Statistics:\n\tTemp Memory Region Size: %s\n\tMax Non-Temp Memory Used: %s\n\t# System Allocations: %s\n\t# Leaked System Allocations",
            memory.temp_region_size,
            sys_stats.most_bytes_allocated,
            sys_stats.total_allocations,
            sys_stats.num_allocations);
    }
}
