module arc.semantic.type;

struct Type {
    import arc.hash: Key;

    enum Kind {
        Aggregate,
        Function,
        Integer,
        Float,
    }

    alias Kind this;

    Key name;
    Kind kind;
}