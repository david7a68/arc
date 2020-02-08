module arc.syntax.location;

alias CharPos = uint;

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
    CharPos start;
    /// The length of the span
    CharPos length;

    CharPos end() const pure { return start + length; }

    Span merge(Span other) const pure {
        auto lo = start < other.start ? start : other.start;
        auto hi_end = end > other.end ? end : other.end;
        return Span(lo, hi_end - lo);
    }
}

@("span:merge") unittest {
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

struct SpannedText {
    Span span;
    const(char)[] text;

    alias span this;

    this(CharPos start, CharPos length, const(char)[] text) pure {
        this.span = Span(start, length);
        this.text = text;
    }
    
    SpannedText get_span(const(char)[] slice) pure {
        assert(text.ptr <= slice.ptr && slice.ptr + slice.length <= text.ptr + text.length);
        
        const offset = cast(uint) (slice.ptr - text.ptr);
        assert(slice.length == 0 || text[offset] == slice[0]);
        
        const length = cast(uint) slice.length;
        assert(offset + length <= text.length);
        
        return SpannedText(cast(uint) (span.start + offset), length, slice);
    }

    const(char)[] get_text(Span text_span) pure {
        const start_index = text_span.start - span.start;
        const end_index = start_index + text_span.length;
        
        assert(start_index + text_span.length <= text.length);
        assert(start_index <= end_index);
        
        return text[start_index .. start_index + text_span.length];
    }

    SourceLoc get_loc(CharPos position) pure {
        uint line_num = 1;
        uint line_pos;
        uint column;

        const slice = text[0 .. position - start];
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

    Span merge(Span other) const pure {
        return span.merge(other);
    }
}

/**
 * A Source represents the source text of a file or autogenerated snippet of
 * code.
 */
struct Source {
    /// The name of the source, or its path
    string name;
    ///
    SpannedText span;
    alias span this;

    bool opBinaryRight(string op = "in")(CharPos pos) pure {
        return start <= pos && pos < end;
    }
}

/**
 * The SourceMap presents a mapping between character positions and the files
 * that they belong to.
 */
struct SourceMap {
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
        CharPos start = sources.length > 0 ? sources[$-1].end : 0;

        if (start + text.length > uint.max)
            assert(false);

        auto src = Source(
            name.dup,
            SpannedText(
                start,
                cast(CharPos) (start + text.length),
                text
            )
        );

        sources ~= src;
        return src;
    }
}
