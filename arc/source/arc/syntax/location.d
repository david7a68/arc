module arc.syntax.location;

alias CharPos = uint;

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

    CharPos end() const { return start + length; }

    Span merge(Span other) const {
        import std.algorithm: min, max;

        auto start = min(this.start, other.start);
        auto end = max(this.start + length, other.start + other.length);

        return Span(start, end);
    }
}

struct SpannedText {
    Span span;
    const(char)[] text;

    alias span this;

    this(Span span, const(char)[] text) {
        assert(text.length == span.length);
        this.span = span;
        this.text = text;
    }

    this(CharPos start, CharPos length, const(char)[] text) {
        this(Span(start, length), text);
    }

    SpannedText opSlice(const(char)* start, CharPos length) {
        assert(text.ptr <= start);
        assert(start + length <= text.ptr + text.length);
        const local_start = cast(uint) (start - text.ptr);
        return SpannedText(local_start, length, text[local_start .. local_start + length]);
    }
    
    SpannedText get_span(const(char)[] text) {
        return opSlice(text.ptr, text.length);
    }

    const(char)[] get_text(Span text_span) {
        const start = text_span.start - span.start;
        assert(start + text_span.length <= text.length);

        return text[start .. text_span.length];
    }

    SpannedText merge(SpannedText other) const {
        import std.algorithm: min, max;
        
        auto span = this.span.merge(other.span);
        auto slice = (cast(const(char)*) min(start, other.start))[0 .. max(start + length, other.start + other.length)];

        return SpannedText(span, slice);
    }

    Span merge(Span other) const {
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
    const(char)[] raw_text;
    /// The absolute span that the source covers
    Span span;
    alias span this;

    bool opBinaryRight(string op = "in")(CharPos pos) {
        return start <= pos && pos < end;
    }

    const(char)[] get_text(Span text_span) {
        const start = text_span.start - span.start;
        assert(start + text_span.length <= raw_text.length);

        return raw_text[start .. text_span.length];
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
    Source reserve(string name, uint source_length) {
        return put(name, new const(char)[](source_length));
    }

    Source put(string name, const(char)[] text) {
        const last = sources[$-1];
        if (last.end + text.length > uint.max)
            assert(false);

        auto src = Source(
            name.dup,
            text,
            Span(last.end, cast(CharPos) (last.end + text.length))
        );

        sources ~= src;
        return src;
    }

    Source get_source(CharPos pos) {
        // binary search for the source
        if (sources.length > 0) {
            size_t left = 0;
            size_t right = sources.length - 1;
            size_t mid = sources.length / 2;

            while (pos !in sources[mid]) {
                mid = (mid + right) / 2;
                if (sources[mid].end < pos)
                    left = mid + 1;
                else if (pos < sources[mid].start)
                    right = mid - 1;
                else
                    return sources[mid];
            }
            assert(0, "unreachable");
        }
        else {
            return Source();
        }
    }

    const(char)[] get_text(Span span) {
        return get_source(span.start).get_text(span);
    }
}
