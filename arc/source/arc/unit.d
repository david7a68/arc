module arc.unit;

import arc.ast;

struct CompilationUnit {
    enum Stage : ubyte {
        uninitialized,
        created,
        loading,
        loaded,
        parsing,
        parsed,

        load_error,
        parse_error,
    }

    Stage stage;
    const char[] path;
    const char[] source;
    SyntaxTree ast;

    this(const char[] source) {
        this.stage = Stage.created;
        this.source = source;
    }
}
