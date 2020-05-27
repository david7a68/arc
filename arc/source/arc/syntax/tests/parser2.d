module arc.syntax.tests.parser2;

import arc.data.ast2;
import arc.data.source: Span;
import arc.syntax.parser2;
import arc.reporter;

import std.stdio: writefln;

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

bool check_error(ParseResult result, ArcError.Code error, size_t count = 1) {
    if (!type_equivalent(result, AstNode.Invalid)) return false;
    
    if (!reporter.has_error(error)) {
        writefln("The parser did not encounter the expected error.");
        writefln("Source: %s\nExpected: %s\nErrors: %s", result.text, error, reporter.errors);
        return false;
    }

    if (reporter.errors.length != count) {
        writefln("The number of errors encountered was unexpected.\nSource: %s\nExpected:    %s\nEncountered: %s\nErrors:\n\t%s", result.text, count, reporter.errors.length, reporter.errors);
        return false;
    }

    return true;
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

@("Parse Assign") unittest {
    with (AstNode.Kind) {
        assert(check_types("a = b;".parse!"statement"(), Assign, Name, Name));
        assert(check_types("() = 1;".parse!"statement"(), Assign, List, Integer));
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

@("Parse Definition") unittest {
    with (AstNode.Kind) {
        {
            auto type = "def T : T2;".parse!"statement"();
            assert(type.span == Span(0, 11));
            assert(check_types(type,
                Definition,
                    Name,
                    Name,
                    Inferred
            ));
        }

        {
            auto type = "def T : (u32, k: u32);".parse!"statement"();
            assert(type.span == Span(0, 22));
            assert(check_types(type,
                Definition,
                    Name,
                    List,
                        Variable,
                            None,
                            Name,
                            Inferred,
                        Variable,
                            Name,
                            Name,
                            Inferred,
                    Inferred,
            ));
        }

        {
            auto error = "def T : (!);".parse!"statement"();
            assert(error.span == Span(0, 11)); // We don't get to the semicolon
            assert(check_error(error, ArcError.TokenExpectMismatch, 1));
        }

        {
            auto decl = "def T := 1;".parse!"statement"();
            assert(check_types(decl,
                Definition,
                    Name,
                    Inferred,
                    Integer,
            ));
        }

        {
            auto decl = "def T : a = b;".parse!"statement"();
            assert(check_types(decl,
                Definition,
                    Name,
                    Name,
                    Name,
            ));
        }

        {
            auto error = "def T := +;".parse!"statement"();
            assert(error.span == Span(0, 10)); // We don't get to the semicolon
            assert(type_equivalent(error, Invalid));
        }
    }
}

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

        // ! cannot start type
        assert(check_error("a : (!);".parse!"statement"(), ArcError.TokenExpectMismatch));

        // ! cannot start type, unexpected EOF
        assert(check_error("a : !".parse!"statement"(), ArcError.TokenExpectMismatch));
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

@("Parse List Error") unittest { // List variable must have type
    assert(check_error("(a = b)".parse!"expression"(), ArcError.TokenExpectMismatch));
    assert(check_error("(a, !)".parse!"expression"(), ArcError.TokenExpectMismatch));
}

@("Parse Call") unittest {
    with (AstNode.Kind) {
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
//  We don't care what error occurs, as long as one does
    assert(check_types(err, AstNode.Invalid));
}

// ----------------------------------------------------------------------
//   _______                       
//  |__   __|                      
//     | | _   _  _ __    ___  ___ 
//     | || | | || '_ \  / _ \/ __|
//     | || |_| || |_) ||  __/\__ \
//     |_| \__, || .__/  \___||___/
//          __/ || |               
//         |___/ |_|               
// ----------------------------------------------------------------------

@("Parse Types") unittest {
    with (AstNode.Kind) {
        assert(check_types("a".parse!"type"(), Name));

        assert(check_types("a.b".parse!"type"(), Access, Name, Name));

        assert(check_types("(a)".parse!"type"(), List, Variable, None, Name, Inferred));

        assert(check_types("(a:b)".parse!"type"(), List, Variable, Name, Name, Inferred));

        assert(check_types("(a := 3)".parse!"type"(), List, Variable, Name, Inferred, Integer));
    }
}

@("Parse Types Error") unittest {
    with (AstNode.Kind) {
        {
            auto err = "(a : !)".parse!"type"();
            assert(check_error(err, ArcError.TokenExpectMismatch));
        }

        {
            auto err = "3".parse!"type"();
            assert(check_error(err, ArcError.TokenExpectMismatch));
        }
    }
}

