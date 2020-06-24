module arc.source;

import arc.data.span;

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
    size_t start_offset;

    this(const char[] path, const char[] text, size_t start_offset) {
        this.path = path;
        this.text = text;
        this.start_offset = start_offset;
    }

    /// The span encompassing this source file.
    auto span() const {
        return Span(start_offset, text.length);
    }

    /// Retrieve the source location of this global position.
    SourceLoc get_loc(uint position)
    in(start_offset <= position && position <= span.end) {
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
