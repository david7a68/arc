module arc.data.span;

/**
 A compressed span, indexed from a global file map such that every character 
 during compilation has a unique index.

 In its compressed format, the span supports a total compilation size of 128
 gibibytes, and a per-file size of up to 128 mebibytes. This should be plenty
 of space for now. If there ever comes a time where larger spans are needed,
 it should be simple to use a large virtual array to hold expanded spans indexed by `start`. 128 billion large spans should be enough, right?
 */
struct Span {
    import std.bitmanip : bitfields;

    // 128 gib
    enum size_t num_start_bits = 37;
    enum size_t max_start = (2 ^^ num_start_bits) - 1;
    // 128 mib
    enum size_t num_length_bits = 27;
    enum size_t max_length = (2 ^^ num_length_bits) - 1;

public:
    mixin(bitfields!(size_t, "start", num_start_bits, size_t, "length", num_length_bits));

    this(size_t start, size_t length)
    in(start < max_start && length < max_length) {
        this.start = start;
        this.length = length;
    }

    size_t end() const {
        return start + length;
    }

    int opCmp(Span rhs) const {
        if (start < rhs.start)
            return -1;
        if (start > rhs.start)
            return 1;
        return 0;
    }

    Span opBinary(string op = "+")(Span rhs) const {
        import std.algorithm : max, min;

        if (this == Span())
            return rhs == Span() ? this : rhs;

        const lo = min(start, rhs.start);
        return Span(lo, max(end, rhs.end) - lo);
    }

    void opOpAssign(string op = "+")(Span other) {
        if (this == Span()) {
            if (other == Span())
                return;
            this = other;
        }
        else {
            this = this + other;
        }
    }
}

/**
 * Merges all non-0 spans together or returns the 0 span if all spans are 0.
 */
auto merge_all(Span[] spans...) {
    Span result;
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
