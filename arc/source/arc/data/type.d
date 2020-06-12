module arc.data.type;

abstract class ArcType {
    enum Kind {
        Unknown,
        Aggregate,
        Function,
        ConstInteger,
        Integer,
        Char,
        Float,
        Pointer,
        Symbol,
    }

    Kind kind;
    size_t size;

    this(Kind kind) {
        this.kind = kind;
    }
}
