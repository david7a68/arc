module arc.log_out;

import core.stdc.stdio : printf;
import shard.buffer_writer : TypedWriter;
import shard.logger : LogLevel, LogEvent, LogEventSink;
import std.format : formattedWrite;

final class CompilerOut : LogEventSink {
    this(bool colorize) {
        _colorized = colorize;
    }

    override void begin_logging() {}

    override void end_logging() {}

    override void flush() {}

    override void log_event(in LogEvent event) {
        char[LogEvent.max_log_length] buffer;
        auto writer = TypedWriter!char(buffer);

        try {
            if (_colorized)
                writer.put(colors[event.level]);
            
            debug writer.formattedWrite!"{%s:%s} "(event.module_name, event.line);
            if (event.level > LogLevel.All)
                writer.formattedWrite!"%s: "(event.level);
            writer.put(event.message[0 .. event.message_length]);
            writer.put(clear_colors);
            writer.put("\n\0");

            () @trusted { printf(&writer.data()[0]); } ();
        }
        catch (Exception e)
            return;
    }

private:
    // dfmt off
    static immutable colors = [
        ""       ,  // LogLevel.All
        "\033[0m",  // LogLevel.Trace
        "\033[96m", // LogLevel.Info
        "\033[33m", // LogLevel.Warn
        "\033[31m", // LogLevel.Error
        "\033[91m", // LogLevel.Fatal
    ];

    static immutable text = [
        "",         // LogLevel.All
        "[Trace] ", // LogLevel.Trace
        "[Info ] ", // LogLevel.Info
        "[Warn ] ", // LogLevel.Warn
        "[Error] ", // LogLevel.Error
        "[Fatal] ", // LogLevel.Fatal
    ];
    // dfmt on

    static immutable clear_colors = "\033[0m";

    bool _colorized;
}
