module tests.arc.parser;

import arc.data.ast;
import arc.data.span : Span;
import arc.syntax.parser;
import arc.syntax.syntax_allocator;
import arc.reporter;
import arc.memory : VirtualMemory, mib;
import std.stdio : writefln;

Reporter reporter;
ParsingContext parser;
VirtualMemory ast_memory;
SyntaxAllocator nodes = void;

static this() {
    ast_memory = VirtualMemory(1.mib);
    nodes = new SyntaxAllocator(&ast_memory);
    parser = ParsingContext(&reporter, nodes);
}

struct ParseResult {
    AstNode* tree;
    alias tree this;
    string text;
}

auto parse(string op)(string text) {
    parser.begin(text);
    reporter.clear();
    mixin("return ParseResult(" ~ op ~ "(&parser), text);");
}

bool type_equivalent(ParseResult result, AstNode.Kind[] types...) {
    import std.algorithm : equal;

    AstNode.Kind[] tree_types;

    void flatten(AstNode* n) {
        if (n is null)
            return;
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
    if (!type_equivalent(result, AstNode.Kind.Invalid))
        return false;

    if (!reporter.has_error(error)) {
        writefln("The parser did not encounter the expected error.");
        writefln("Source: %s\nExpected: %s\nErrors: %s", result.text, error, reporter.errors);
        return false;
    }

    if (reporter.errors.length != count) {
        writefln("The number of errors encountered was unexpected."
                ~ "\nSource: %s\nExpected:    %s\nEncountered: %s\nErrors:\n\t%s",
                result.text, count, reporter.errors.length, reporter.errors);
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
        assert(check_types("a = b;".parse!"stmt"(), Assign, Name, Name));
        assert(check_types("() = 1;".parse!"stmt"(), Assign, List, Integer));
        assert(check_types("a.b = 3;".parse!"stmt"(), Assign, Access, Name, Name, Integer));
    }
}

@("Parse Block") unittest {
    with (AstNode.Kind) {
        assert("{ ".parse!"stmt"().is_valid == false);
        assert("{ a }".parse!"stmt"().is_valid == false);
        assert("{ ( } )".parse!"stmt"().is_valid == false);
        assert("( a, { ) }".parse!"stmt"().is_valid == false);
    }
}

@("Parse Definition") unittest {
    with (AstNode.Kind) {
        auto type = "def T : T2;".parse!"stmt"();
        assert(type.span == Span(0, 11));
        assert(check_types(type,
            Definition,
                Name,
                Name,
                Inferred
        ));
    }

    with (AstNode.Kind) {
        auto type = "def T : (u32, k: u32);".parse!"stmt"();
        assert(type.span == Span(0, 22));
        assert(check_types(type,
            Definition,
                Name,
                List,
                    ListMember,
                        None,
                        Name,
                        Inferred,
                    ListMember,
                        Name,
                        Name,
                        Inferred,
                Inferred,
        ));
    }

    with (AstNode.Kind) {
        auto error = "def T : (!);".parse!"stmt"();
        assert(error.span == Span(0, 11));
        assert(check_error(error, ArcError.TokenExpectMismatch, 1));
    }

    with (AstNode.Kind) {
        auto decl = "def T := 1;".parse!"stmt"();
        assert(check_types(decl,
            Definition,
                Name,
                Inferred,
                Integer,
        ));
    }

    with (AstNode.Kind) {
        auto decl = "def T : a = b;".parse!"stmt"();
        assert(check_types(decl,
            Definition,
                Name,
                Name,
                Name,
        ));
    }

    with (AstNode.Kind) {
        assert(check_types("def F := () -> {};".parse!"stmt"(),
            Definition,
                Name,
                Inferred,
                Function,
                    List,
                    Inferred,
                    Block
        ));
    }

    with (AstNode.Kind) {
        auto error = "def T := +;".parse!"stmt"();
        assert(error.span == Span(0, 10)); // We don't get to the semicolon
        assert(type_equivalent(error, Invalid));
    }
}

@("Parse Escape") unittest {
    with (AstNode.Kind)
    assert(check_types("break;".parse!"stmt"(), Break));

    with (AstNode.Kind)
    assert(check_types("continue;".parse!"stmt"(), Continue));
}

@("Parse If") unittest {
    with (AstNode.Kind)
    assert(check_types("if a {} else if b {} else c;".parse!"stmt"(),
        If,
            Name,
            Block,
            If,
                Name,
                Block,
                Name
    ));
}

@("Parse Loop") unittest {
    with (AstNode.Kind)
    assert(check_types("loop { break;}".parse!"stmt"(), Loop, Block, Break));
}

@("Parse Variable") unittest {
    with (AstNode.Kind) {
        {
            auto var = "a : T = 3;".parse!"stmt"();
            assert(var.span == Span(0, 10));
            assert(check_types(var,
                Variable,
                    Name,
                    Name,
                    Integer
            ));
        }

        {
            auto var = "a := 3;".parse!"stmt"();
            assert(var.span == Span(0, 7));
            assert(check_types(var,
                Variable,
                    Name,
                    Inferred,
                    Integer
            ));
        }

        {
            auto var = "a : T;".parse!"stmt"();
            assert(var.span == Span(0, 6));
            assert(check_types(var,
                Variable,
                    Name,
                    Name,
                    Inferred
            ));
        }

        // ! cannot start type
        assert(check_error("a : (!);".parse!"stmt"(), ArcError.TokenExpectMismatch));

        // ! cannot start type, unexpected EOF
        assert(check_error("a : !".parse!"stmt"(), ArcError.TokenExpectMismatch));
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
    assert(check_types("a_name".parse!"expr", AstNode.Kind.Name));
}

@("Parse Int") unittest {
    assert(check_types("20".parse!"expr", AstNode.Kind.Integer));
}

@("Parse Char") unittest {
    assert(check_types("'a'".parse!"expr", AstNode.Kind.Char));
}

@("Parse String") unittest {
    assert(check_types(`"Ha"`.parse!"expr", AstNode.Kind.String));
}

@("Parse Unary") unittest {
    with (AstNode.Kind) {
        assert(check_types("-var".parse!"expr", Negate, Name));
    }
}

@("Parse Binary") unittest {
    with (AstNode.Kind) {
        assert(check_types("1 + 1".parse!"expr", Add, Integer, Integer));
        assert(check_types("a ^ 2".parse!"expr", Power, Name, Integer));

        assert(check_types("-a * b".parse!"expr",
            Multiply,
                Negate,
                    Name,
                Name
        ));

        assert(check_types("a - -3".parse!"expr",
            Subtract,
                Name,
                Negate,
                    Integer
        ));

        assert(check_types("b - c + d * e ^ f ^ g / h".parse!"expr",
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

        assert(check_types("a.b()::c::d".parse!"expr",
            StaticAccess,
                StaticAccess,
                    Call,
                        Access,
                            Name,
                            Name,
                        List,
                    Name,
                Name
        ));
    }
}

@("Parse Empty List") unittest {
    with (AstNode.Kind) {
        auto list = "()".parse!"expr"();
        assert(list.span == Span(0, 2));
        assert(check_types(list, List));
    }
}

@("Parse List Members") unittest {
    with (AstNode.Kind) {
        auto list = "[a:= 2, 3, c:T=blah.b]".parse!"expr"();
        assert(list.span == Span(0, 22));
        assert(check_types(list,
            List,
                ListMember,
                    Name,
                    Inferred,
                    Integer,
                ListMember,
                    None,
                    Inferred,
                    Integer,
                ListMember,
                    Name,
                    Name,
                    Access,
                        Name,
                        Name
        ));
    }

    with (AstNode.Kind) {
        assert(check_types("(a: T[])".parse!"expr"(),
            List,
                ListMember,
                    Name,
                    Call,
                        Name,
                        List,
                    Inferred
        ));
    }
}

@("Parse List Error") unittest { // List variable must have type
    assert(check_error("(a = b)".parse!"expr"(), ArcError.TokenExpectMismatch));
    assert(check_error("(a, !)".parse!"expr"(), ArcError.TokenExpectMismatch));
}

@("Parse Import") unittest {
    with (AstNode.Kind) {
        assert(check_types("import a".parse!"expr"(), Import, Name));
        assert(check_types("import a::b".parse!"expr"(), Import, StaticAccess, Name, Name));
        assert(check_types("import a()".parse!"expr"(), Import, Call, Name, List));
        assert(check_types("(import b)::c()".parse!"expr"(),
            Call,
                StaticAccess,
                    List,
                        ListMember,
                            None,
                            Inferred,
                            Import,
                                Name,
                    Name,
                List));
    }
}

@("Parse Call") unittest {
    with (AstNode.Kind) {
        // a 4 -> a(4)
        assert(check_types("a a".parse!"expr"(), Call, Name, Name));
        assert(check_types("a 4".parse!"expr"(), Call, Name, Integer));
        assert(check_types("a 'a'".parse!"expr"(), Call, Name, Char));
        assert(check_types("a \"\"".parse!"expr"(), Call, Name, String));

        assert(check_types("a * b()".parse!"expr"(),
            Multiply,
                Name,
                Call,
                    Name,
                    List
        ));

        // a b c -> (a b) c
        assert(check_types("a b c".parse!"expr"(), Call, Call, Name, Name, Name));
    }
}

@("Parse Function (Simple)") unittest {
    with (AstNode.Kind) {
        assert(check_types("(a) -> b".parse!"expr"(),
            Function,
                List,
                    ListMember,
                        None,
                        Inferred,
                        Name,
                Inferred,
                Name
        ));

        assert(check_types("() -> a * b".parse!"expr"(),
            Function,
                List,
                Inferred,
                Multiply,
                    Name,
                    Name
        ));
    }
}

@("Parse Function (Complex Return)") unittest {
    with (AstNode.Kind) {
        // (a) -> ((b) -> c)
        assert(check_types("(a) -> (b) -> c".parse!"expr"(),
            Function,
                List,
                    ListMember,
                        None,
                        Inferred,
                        Name,
                Inferred,
                Function,
                    List,
                        ListMember,
                            None,
                            Inferred,
                            Name,
                    Inferred,
                    Name
        ));
    }
}

@("Parse Function (With Block)") unittest {
    with (AstNode.Kind) assert(check_types("(a) -> {}".parse!"expr"(),
        Function,
            List,
                ListMember,
                    None,
                    Inferred,
                    Name,
                Inferred,
                Block
    ));
}

@("Parse Function Error") unittest {
    auto err = "() ->;".parse!"expr"();
    //  We don't care what error occurs, as long as one does
    assert(check_types(err, AstNode.Kind.Invalid));
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

        assert(check_types("(a)".parse!"type"(), List, ListMember, None, Name, Inferred));

        assert(check_types("(a:b)".parse!"type"(), List, ListMember, Name, Name, Inferred));

        assert(check_types("(a := 3)".parse!"type"(), List, ListMember, Name, Inferred, Integer));

        assert(check_types("*T".parse!"type"(), PointerType, Name));

    }
}

@("Parse Types Error") unittest {
    with (AstNode.Kind) {
        assert(check_error("(a : !)".parse!"type"(), ArcError.TokenExpectMismatch));
        assert(check_error("3".parse!"type"(), ArcError.TokenExpectMismatch));
        assert(check_error("a.!".parse!"type"(), ArcError.TokenExpectMismatch));
    }
}

// ----------------------------------------------------------------------
//   ______                              
//  |  ____|                             
//  | |__    _ __  _ __  ___   _ __  ___ 
//  |  __|  | '__|| '__|/ _ \ | '__|/ __|
//  | |____ | |   | |  | (_) || |   \__ \
//  |______||_|   |_|   \___/ |_|   |___/
//    
// ----------------------------------------------------------------------

@("Parser Resynchronization")
unittest {
    with (AstNode.Kind) {
        const stmt = "{{{{{def a := !!}}anything at all}}}\nblah();".parse!"stmt";
        assert(stmt.span == Span(0, 17));
        assert(!parser.is_done);
    }
}
