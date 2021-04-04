module arc.data.span;

/**
 Represents a span of text from within a file. Spans are file-specific, and
 cannot be merged or compared across files.
 */
struct Span {
    uint start;
    uint length;

    this(uint start, uint length) {
        this.start = start;
        this.length = length;
    }

    uint end() const {
        return start + length;
    }

    int opCmp(Span rhs) const {
        return start < rhs.start ? -1 : (start > rhs.start ? 1 : 0);
    }

    Span opBinary(string op = "+")(Span rhs) const {
        import std.algorithm : max, min;

        if (this == Span())
            return rhs == Span() ? this : rhs;

        const lo = min(start, rhs.start);
        return Span(lo, max(end, rhs.end) - lo);
    }

    void opOpAssign(string op = "+")(Span rhs) {
        if (this == Span()) {
            if (rhs == Span())
                return;
            this = rhs;
        }
        else {
            this = this + rhs;
        }
    }
}

/**
 * Merges all non-0 spans together or returns the 0 span if all spans are 0.
 */
auto merge_all(Span[] spans...) {
    auto result = Span(0, 0);
    foreach (span; spans)
        result += span;
    return result;
}

@("merge spans") unittest {
    auto a = Span(10, 20);
    auto b = Span(5, 15);
    auto c = Span(15, 25);
    auto d = Span(5, 25);
    assert(a + a == Span(10, 20));
    assert(a + b == Span(5, 25));
    assert(a + c == Span(10, 30));
    assert(a + d == Span(5, 25));
    assert(b + c == Span(5, 35));

    assert(merge_all(a, b, c, d) == Span(5, 35));
    assert(merge_all(Span(), Span()) == Span());
}
