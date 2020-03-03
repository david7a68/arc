module arc.source;

struct SourceLoc {
    uint line;
    uint line_position;
    uint column;
}

/**
 * A Span represents a contiguous region of code starting from `start` and
 * ending at `start + length` exclusive.
 *
 * The lack of a reference to the file that this span belongs to necessitates
 * that the information be stored elsewhere.
 */
struct Span {
    /// The index of the first character in the span
    uint start;
    /// THe length of the span
    uint length;

    auto end() const { return cast(uint) (start + length); }

}

auto merge(Span lhs, Span rhs) {
    auto lo = lhs.start < rhs.start ? lhs.start : rhs.start;
    auto hi = lhs.end > rhs.end ? lhs.end : rhs.end;
    return Span(lo, hi - lo);
}

@("merge spans") unittest {
    auto a = Span(10, 20);
    auto b = Span(5, 15);
    auto c = Span(15, 25);
    auto d = Span(5, 25);
    assert(a.merge(a) == Span(10, 20));
    assert(a.merge(b) == Span(5, 25));
    assert(a.merge(c) == Span(10, 30));
    assert(a.merge(d) == Span(5, 25));
    assert(b.merge(c) == Span(5, 35));
}

Span merge_all(Span[] spans...) {
    import std.algorithm: filter, reduce;

    return spans.filter!(a => a != Span()).reduce!merge;
}

/**
 * A Source represents the source text of a file or autogenerated snippet of
 * code.
 */
struct Source {
    string name;
    const(char)[] text;
    uint start_offset;

    auto span() const {
        return Span(start_offset, cast(uint) text.length);
    }

    SourceLoc get_loc(uint position) pure {
        uint line_num = 1;
        uint line_pos;
        uint column;

        const slice = text[0 .. position - start_offset];
        foreach (i, c; slice) {
            if (c == '\n') {
                column = 0;
                line_num++;
                line_pos = cast(uint) i;
            }
            column++;
        }

        return SourceLoc(line_num, line_pos, column);
    }

    bool opBinaryRight(string op = "in")(Span span) {
        return start_offset <= span.start && span.end <= this.span.end;
    }
}

/**
 * The SourceMap presents a mapping between character positions and the files
 * that they belong to.
 */
class SourceMap {
    Source[] sources;

    /**
     * Reserve space for a source of size `source_length`.
     *
     * Returns: the allocated character array to be filled
     */
    Source reserve(string name, uint source_length) pure {
        return put(name, new const(char)[](source_length));
    }

    Source put(const(char)[] name, const(char)[] text) pure {
        uint start = sources.length > 0 ? cast(uint) (sources[$-1].start_offset + sources[$-1].text.length) : 0;

        if (start + text.length > uint.max)
            assert(false);

        auto src = Source(
            name.dup,
            text,
            start
        );

        sources ~= src;
        return src;
    }

    const(char)[] get_spanned_text(Span span) {
        if (span == Span())
            return [];

        auto source = () {
            foreach (src; sources)
                if (span in src)
                    return src;
            assert(false);
        } ();

        const start = span.start - source.start_offset;
        return source.text[start .. start + span.length];
    }
}