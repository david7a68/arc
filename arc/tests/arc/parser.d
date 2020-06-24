module tests.arc.parser;


import arc.data.ast;
import arc.data.span;
import arc.data.symbol;
import arc.memory : mib, VirtualMemory, ArrayPool;
import arc.reporter;
import arc.syntax.parser;
import std.stdio : writefln;

VirtualMemory vm;
ArrayPool!(AstNode*) arrays;

Parser parser;
Reporter reporter;
SymbolTable symbol_table;

static this() {
    // TODO: Find out if we can reduce memory usage
    vm = VirtualMemory(2.mib);
    arrays = ArrayPool!(AstNode*)(&vm);

    parser = Parser();
    reporter = Reporter();
    symbol_table = SymbolTable(&vm);
}

struct ParseResult {
    AstNode* tree;
    alias tree this;
    string text;
}

auto parse(string op)(string text) {
    parser.begin(ParseUnit(&vm, &arrays, &reporter, &symbol_table, text));
    scope(exit) parser.end();

    reporter.clear();
    mixin("return ParseResult(parser." ~ op ~ "(), text);");
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
        assert(check_types("a = b;".parse!"stmt"(), Assign, SymbolRef, SymbolRef));
        assert(check_types("() = 1;".parse!"stmt"(), Assign, List, Integer));
        assert(check_types("a.b = 3;".parse!"stmt"(), Assign, Access, SymbolRef, SymbolRef, Integer));
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
                SymbolRef,
                Inferred
        ));
    }

    with (AstNode.Kind) {
        auto type = "def T : (u32, k: u32);".parse!"stmt"();
        assert(type.span == Span(0, 22));
        assert(check_types(type,
            Definition,
                List,
                    ListMember,
                        SymbolRef,
                        Inferred,
                    ListMember,
                        SymbolRef,
                        Inferred,
                Inferred,
        ));
    }

    with (AstNode.Kind) {
        auto decl = "def T := 1;".parse!"stmt"();
        assert(check_types(decl,
            Definition,
                Inferred,
                Integer,
        ));
    }

    with (AstNode.Kind) {
        auto decl = "def T : a = b;".parse!"stmt"();
        assert(check_types(decl,
            Definition,
                SymbolRef,
                SymbolRef,
        ));
    }

    with (AstNode.Kind) {
        assert(check_types("def F := () -> {};".parse!"stmt"(),
            Definition,
                Inferred,
                Function,
                    List,
                    Inferred,
                    Block
        ));
    }
}

@("Parse Escape") unittest {
    with (AstNode.Kind)
    assert(check_types("break;".parse!"stmt"(), Break));

    with (AstNode.Kind)
    assert(check_types("continue;".parse!"stmt"(), Continue));
}

@("Parse Return") unittest {
    with (AstNode.Kind)
    assert(check_types("return;".parse!"stmt"(), Return, None));

    with (AstNode.Kind)
    assert(check_types("return a;".parse!"stmt"(), Return, SymbolRef));
}

@("Parse If") unittest {
    with (AstNode.Kind)
    assert(check_types("if a {} else if b {} else c;".parse!"stmt"(),
        If,
            SymbolRef,
            Block,
            If,
                SymbolRef,
                Block,
                SymbolRef
    ));
}

@("Parse Loop") unittest {
    with (AstNode.Kind)
    assert(check_types("loop { break;}".parse!"stmt"(), Loop, Break));
}

@("Parse Variable") unittest {
    with (AstNode.Kind) {
        {
            auto var = "a : T = 3;".parse!"stmt"();
            assert(var.span == Span(0, 10));
            assert(check_types(var,
                Variable,
                    SymbolRef,
                    Integer
            ));
        }

        {
            auto var = "a := 3;".parse!"stmt"();
            assert(var.span == Span(0, 7));
            assert(check_types(var,
                Variable,
                    Inferred,
                    Integer
            ));
        }

        {
            auto var = "a : T;".parse!"stmt"();
            assert(var.span == Span(0, 6));
            assert(check_types(var,
                Variable,
                    SymbolRef,
                    Inferred
            ));
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

@("Parse SymbolRef") unittest {
    assert(check_types("a_SymbolRef".parse!"expr", AstNode.Kind.SymbolRef));
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
        assert(check_types("-var".parse!"expr", Negate, SymbolRef));
    }
}

@("Parse Binary") unittest {
    with (AstNode.Kind) {
        assert(check_types("1 + 1".parse!"expr", Add, Integer, Integer));
        assert(check_types("a ^ 2".parse!"expr", Power, SymbolRef, Integer));

        assert(check_types("-a * b".parse!"expr",
            Multiply,
                Negate,
                    SymbolRef,
                SymbolRef
        ));

        assert(check_types("a - -3".parse!"expr",
            Subtract,
                SymbolRef,
                Negate,
                    Integer
        ));

        assert(check_types("b - c + d * e ^ f ^ g / h".parse!"expr",
            Add,
                Subtract,
                    SymbolRef,                   // b
                    SymbolRef,                   // c
                Divide,
                    Multiply,
                        SymbolRef,               // d
                        Power,
                            SymbolRef,           // e
                            Power,
                                SymbolRef,       // f
                                SymbolRef,       // g
                    SymbolRef                    // h
        ));

        assert(check_types("a.b()::c::d".parse!"expr",
            StaticAccess,
                StaticAccess,
                    Call,
                        Access,
                            SymbolRef,
                            SymbolRef,
                        List,
                    SymbolRef,
                SymbolRef
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
                    Inferred,
                    Integer,
                ListMember,
                    Inferred,
                    Integer,
                ListMember,
                    SymbolRef,
                    Access,
                        SymbolRef,
                        SymbolRef
        ));
    }

    with (AstNode.Kind) {
        assert(check_types("(a: T[])".parse!"expr"(),
            List,
                ListMember,
                    Call,
                        SymbolRef,
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
        assert(check_types("import a".parse!"expr"(), Import, SymbolRef));
        assert(check_types("import a::b".parse!"expr"(), Import, StaticAccess, SymbolRef, SymbolRef));
        assert(check_types("import a()".parse!"expr"(), Import, Call, SymbolRef, List));
        assert(check_types("(import b)::c()".parse!"expr"(),
            Call,
                StaticAccess,
                    List,
                        ListMember,
                            Inferred,
                            Import,
                                SymbolRef,
                    SymbolRef,
                List));
    }
}

@("Parse Call") unittest {
    with (AstNode.Kind) {
        // a 4 -> a(4)
        assert(check_types("a a".parse!"expr"(), Call, SymbolRef, SymbolRef));
        assert(check_types("a 4".parse!"expr"(), Call, SymbolRef, Integer));
        assert(check_types("a 'a'".parse!"expr"(), Call, SymbolRef, Char));
        assert(check_types("a \"\"".parse!"expr"(), Call, SymbolRef, String));

        assert(check_types("a * b()".parse!"expr"(),
            Multiply,
                SymbolRef,
                Call,
                    SymbolRef,
                    List
        ));

        // a b c -> (a b) c
        assert(check_types("a b c".parse!"expr"(), Call, Call, SymbolRef, SymbolRef, SymbolRef));
    }
}

@("Parse Function (Simple)") unittest {
    with (AstNode.Kind) {
        assert(check_types("(a) -> b".parse!"expr"(),
            Function,
                List,
                    ListMember,
                        Inferred,
                        SymbolRef,
                Inferred,
                SymbolRef
        ));

        assert(check_types("() -> a * b".parse!"expr"(),
            Function,
                List,
                Inferred,
                Multiply,
                    SymbolRef,
                    SymbolRef
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
                        Inferred,
                        SymbolRef,
                Inferred,
                Function,
                    List,
                        ListMember,
                            Inferred,
                            SymbolRef,
                    Inferred,
                    SymbolRef
        ));
    }
}

@("Parse Function (With Block)") unittest {
    with (AstNode.Kind) assert(check_types("(a) -> {}".parse!"expr"(),
        Function,
            List,
                ListMember,
                    Inferred,
                    SymbolRef,
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
        assert(check_types("a".parse!"type"(), SymbolRef));

        assert(check_types("a.b".parse!"type"(), Access, SymbolRef, SymbolRef));

        assert(check_types("(a)".parse!"type"(), List, ListMember, SymbolRef, Inferred));

        assert(check_types("(a:b)".parse!"type"(), List, ListMember, SymbolRef, Inferred));

        assert(check_types("(a := 3)".parse!"type"(), List, ListMember, Inferred, Integer));

        assert(check_types("*T".parse!"type"(), PointerType, SymbolRef));
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
