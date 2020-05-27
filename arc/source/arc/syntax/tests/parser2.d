module arc.syntax.tests.parser2;

import arc.data.ast2;
import arc.data.source: Span;
import arc.syntax.parser2;
import arc.reporter;

Reporter reporter;
ParsingContext parser;
AstNodeAllocator nodes = void;

static this() {
    nodes = new AstNodeAllocator();
    parser = ParsingContext(&reporter, &nodes);
}

struct ParseResult {
    AstNode* tree;
    alias tree this;
    string text;
}

auto parse(string op)(string text) {
    parser.begin(text);
    reporter.clear();
    mixin ("return ParseResult(parse_" ~ op ~ "(&parser), text);");
}

bool type_equivalent(ParseResult result, AstNode.Kind[] types...) {
    import std.algorithm: equal;
    import std.stdio: writefln;

    AstNode.Kind[] tree_types;

    void flatten(AstNode* n) {
        if (n is null) return;
        tree_types ~= n.kind;

        foreach (child; n.children)
            flatten(child);
    }

    flatten(result.tree);
    if (equal(types, tree_types))
        return true;
    
    writefln(
        "Test failed: The tree is not equal to the specified types.\n" ~ 
        "Source: %s\n" ~
        "Tree:\n" ~
        "\t%s\n" ~
        "Expected:\n" ~
        "\t%s", result.text, tree_types, types);
    return false;
}

bool check_types(ParseResult result, AstNode.Kind[] types...) {
    return type_equivalent(result, types);
}

// ----------------------------------------------------------------------
//    _____  _          _                                 _        
//   / ____|| |        | |                               | |       
//  | (___  | |_  __ _ | |_  ___  _ __ ___    ___  _ __  | |_  ___ 
//   \___ \ | __|/ _` || __|/ _ \| '_ ` _ \  / _ \| '_ \ | __|/ __|
//   ____) || |_| (_| || |_|  __/| | | | | ||  __/| | | || |_ \__ \
//  |_____/  \__|\__,_| \__|\___||_| |_| |_| \___||_| |_| \__||___/
//
// ----------------------------------------------------------------------

@("Parse Variable") unittest {
    with (AstNode.Kind) {
        {
            auto var = "a : T = 3;".parse!"statement"();
            assert(var.span == Span(0, 10));
            assert(check_types(var,
                Variable,
                    Name,
                    Name,
                    Integer
            ));
        }

        {
            auto var = "a := 3;".parse!"statement"();
            assert(var.span == Span(0, 7));
            assert(check_types(var,
                Variable,
                    Name,
                    Inferred,
                    Integer
            ));
        }

        {
            auto var = "a : T;".parse!"statement"();
            assert(var.span == Span(0, 6));
            assert(check_types(var,
                Variable,
                    Name,
                    Name,
                    Inferred
            ));
        }

        {
            auto var = "a : !;".parse!"statement"();
            assert(reporter.has_error(ArcError.TokenExpectMismatch));
            assert(type_equivalent(var,
                Variable, Name, Invalid, Inferred
            ));
        }

        {
            auto var = "a : !".parse!"statement"();
            assert(reporter.has_error(ArcError.UnexpectedEndOfFile));
            assert(type_equivalent(var,
                Invalid
            ));
        }
    }
}

@("Parse Block") unittest {
    with (AstNode.Kind) {
        assert(check_types("{}".parse!"statement"(), Block));

        {
            auto block = "{ a; }".parse!"statement"();
            assert(block.span == Span(0, 6));
            assert(check_types(block, Block, Name));
        }

        {
            const block = "{ ".parse!"statement"();
            assert(!block.is_valid);
        }

        {
            const block = "{ a }".parse!"statement"();
            assert(!block.is_valid);
        }

        {
            const block = "( a, { ) }".parse!"statement"();
            assert(!block.is_valid);
        }

        {
            const block = "{ ( } )".parse!"statement"();
            assert(!block.is_valid);
        }
    }
}

// ----------------------------------------------------------------------
//   ______                                    _                    
//  |  ____|                                  (_)                   
//  | |__   __  __ _ __   _ __  ___  ___  ___  _   ___   _ __   ___ 
//  |  __|  \ \/ /| '_ \ | '__|/ _ \/ __|/ __|| | / _ \ | '_ \ / __|
//  | |____  >  < | |_) || |  |  __/\__ \\__ \| || (_) || | | |\__ \
//  |______|/_/\_\| .__/ |_|   \___||___/|___/|_| \___/ |_| |_||___/
//                | |                                               
//                |_|                                               
// ----------------------------------------------------------------------

@("Parse Name") unittest {
    assert(check_types("a_name".parse!"expression", AstNode.Kind.Name));
}

@("Parse Int") unittest {
    assert(check_types("20".parse!"expression", AstNode.Kind.Integer));
}

@("Parse Char") unittest {
    assert(check_types("'a'".parse!"expression", AstNode.Kind.Char));
}

@("Parse Unary") unittest {
    with (AstNode.Kind) {
        assert(check_types("-var".parse!"expression", Negate, Name));
    }
}

@("Parse Binary") unittest {
    with (AstNode.Kind) {
        assert(check_types("1 + 1".parse!"expression", Add, Integer, Integer));
        assert(check_types("a ^ 2".parse!"expression", Power, Name, Integer));

        assert(check_types("-a * b".parse!"expression",
            Multiply,
                Negate,
                    Name,
                Name
        ));

        assert(check_types("a - -3".parse!"expression",
            Subtract,
                Name,
                Negate,
                    Integer
        ));

        assert(check_types("b - c + d * e ^ f ^ g / h".parse!"expression",
            Add,
                Subtract,
                    Name,                   // b
                    Name,                   // c
                Divide,
                    Multiply,
                        Name,               // d
                        Power,
                            Name,           // e
                            Power,
                                Name,       // f
                                Name,       // g
                    Name                    // h
        ));
    }
}

@("Parse Empty List") unittest {
    with (AstNode.Kind) {
        auto list = "()".parse!"expression"();
        assert(list.span == Span(0, 2));
        assert(check_types(list, List));
    }
}

@("Parse List Members") unittest {
    with (AstNode.Kind) {
        auto list = "[a:= 2, 3, c:T=blah.b]".parse!"expression"();
        assert(list.span == Span(0, 22));
        assert(check_types(list,
            List,
                Variable,
                    Name,
                    Inferred,
                    Integer,
                Variable,
                    None,
                    Inferred,
                    Integer,
                Variable,
                    Name,
                    Name,
                    Access,
                        Name,
                        Name
        ));
    }
}

@("Parse List Error") unittest {
        auto err = "(a = b)".parse!"expression"();
        assert(reporter.has_error(ArcError.TokenExpectMismatch));
        assert(reporter.errors.length == 1);
        check_types(err, AstNode.Invalid);
}

@("Parse Call") unittest {
    with (AstNode.Kind) {
        // a -3 -> a - 3, not a call
        assert(check_types("a 4".parse!"expression"(), Call, Name, Integer));
        
        assert(check_types("a * b()".parse!"expression"(),
            Multiply,
                Name,
                Call,
                    Name,
                    List
        ));

        // a b c -> a (b c)
        assert(check_types("a b c".parse!"expression"(), Call, Name, Call, Name, Name));
    }
}

@("Parse Function (Simple)") unittest {
    with (AstNode.Kind) {
        assert(check_types("(a) -> b".parse!"expression"(),
            Function,
                List,
                    Variable,
                        None,
                        Inferred,
                        Name,
                Inferred,
                Name
        ));
    }
}

@("Parse Function (Complex Return)") unittest {
    with (AstNode.Kind) {
        // (a) -> ((b) -> c)
        assert(check_types("(a) -> (b) -> c".parse!"expression"(),
            Function,
                List,
                    Variable,
                        None,
                        Inferred,
                        Name,
                Inferred,
                Function,
                    List,
                        Variable,
                            None,
                            Inferred,
                            Name,
                    Inferred,
                    Name
        ));
    }
}

@("Parse Function (With Block)") unittest {
    with (AstNode.Kind) assert(check_types("(a) -> {}".parse!"expression"(),
        Function,
            List,
                Variable,
                    None,
                    Inferred,
                    Name,
                Inferred,
                Block
    ));
}

@("Parse Function Error") unittest {
    auto err = "() ->;".parse!"expression"();
    assert(reporter.errors.length == 1); //  We don't care what error occurs, as long as one does
    assert(check_types(err, AstNode.Invalid));
}
