module arc.data.source_map;

final class SourceMap {
    import arc.data.source : Source, Span;

    // The first source is a reserved dummy source to allow source_of to return
    // an error condition without throwing an exception.
    Source[] sources;

    this() {
        sources = [new Source("dummy", "", 0)];
    }

    /**
     * Add a source file to the map.
     */
    Source put(const char[] path, const char[] text) {
        const start = sources[$ - 1].span.end;

        auto src = new Source(path, text, start);
        sources ~= src;

        return src;
    }

    /**
     * Retrieves the source that this span is part of. If the span is not part
     * of any source, the dummy 0-size source is returned.
     */
    Source source_of(Span span) {
        if (span == Span())
            return sources[0];

        auto source = () {
            foreach (src; sources[1 .. $])
                if (span in src)
                    return src;
            return sources[0];
        }();

        return source;
    }

    /**
     * Retrieves the text spanned by `span` if it exists. Otherwise, returns
     * the empty string.
     */
    const(char[]) get_spanned_text(Span span) {
        auto source = source_of(span);

        if (source !is sources[0]) {
            const start = span.start - source.start_offset;
            return source.text[start .. start + span.length];
        }
        return "";
    }

    auto coordinates_of(Span span) {
        struct Coord {
            size_t line, column;
        }

        auto source = source_of(span);
        const local_offset = span.start - source.start_offset;

        size_t line = 1, column = 0;
        for (auto i = 0; i < local_offset; i++) {
            // handles \r\n
            if (source.text[i] == '\n') {
                line++;
                column = 0;
            }
            else
                column++;
        }

        return Coord(line, column);
    }
}
