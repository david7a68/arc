module arc.data.source;

/**
 * A fixed region of a source text. These are used basically everywhere for
 * error reporting, so they have to be as small as possible. For convenience,
 * a 0-initialized span is considered invalid.
 *
 * A span is indexed from the start of the file map, so that information about
 * the source file may be retrieved by searching through the map.
 */
struct Span {
    /// The global index of the first character in this span
    uint start;
    /// The number of characters in this span
    uint length;

    /// The index of the last character in this span + 1
    auto end() const { return start + length; }
}

/**
 * Merge two spans together. If either span is Span(0, 0), `merge` will return
 * the non-0 span, or if both spans are 0, the 0 span.
 */
auto merge(Span lhs, Span rhs) {
    if (lhs == Span()) {
        if (rhs == Span())
            return lhs;
        else
            return rhs;
    }
    else {
        auto lo = lhs.start < rhs.start ? lhs.start : rhs.start;
        auto hi = lhs.end > rhs.end ? lhs.end : rhs.end;
        return Span(lo, hi - lo);
    }
}

/**
 * Merges all non-0 spans together or returns the 0 span if all spans are 0.
 */
auto merge_all(Span[] spans...) {
    import std.algorithm: filter, fold;

    return spans.filter!(a => a != Span()).fold!merge(Span());
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

    assert(merge_all(a, b, c, d) == Span(5, 35));
    assert(merge_all(Span(), Span()) == Span());
}

/**
 * The SourceLoc bundles together the line/column information of a single
 * character that is used to produce debug messages.
 */
struct SourceLoc {
    /// The source of this information.
    Source source;
    /// The global index of the character at this location.
    size_t index;
    /// The line where the character lies. This is source-local.
    uint line;
    /// The column where the character lies. This is source-local.
    uint column;
}

final class Source {
    /// The file path to the source file. This is local to the current working
    /// directory when the compiler is run.
    const char[] path;
    /// The text content of the source file.
    const char[] text;
    /// The global index of the first character in the source file. This should
    /// never be 0.
    uint start_offset;

    this(const char[] path, const char[] text, uint start_offset) {
        this.path = path;
        this.text = text;
        this.start_offset = start_offset;
    }

    /// The span encompassing this source file.
    auto span() const { return Span(start_offset, cast(uint) text.length); }

    /// Retrieve the source location of this global position.
    SourceLoc get_loc(uint position) in (start_offset <= position && position <= span.end) {
        uint line_num = 1;
        uint column;

        const slice = text[0 .. position - start_offset];

        foreach (i, c; slice) {
            if (c == '\n') {
                column = 1;
                line_num++;
            }
            else
                column++;
        }

        return SourceLoc(this, position, line_num, column);
    }

    /// Tests if the span is enclosed completely within this source file.
    bool opBinaryRight(string op = "in")(Span span) {
        return start_offset <= span.start && span.end <= this.span.end;
    }
}
