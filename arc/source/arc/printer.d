/**
AstPrinter and AstPrettyPrinter
*/
module arc.printer;

import arc.ast;
import shard.memory : Allocator;
import shard.array : Array;

struct AstPrinter {
    enum IndentType : ubyte {
        None,
        Space,
        Bar
    }

    static immutable indent_str = ["NONE", "    ", " │  "];
    enum tbar = " ├─ ";
    enum lbar = " └─ ";

    Array!IndentType stack;
    Array!char data;
    SyntaxTree* ast;

    this(SyntaxTree* ast, Allocator allocator) {
        stack = Array!IndentType(allocator);
        data = Array!char(allocator);
        this.ast = ast;
    }
}
