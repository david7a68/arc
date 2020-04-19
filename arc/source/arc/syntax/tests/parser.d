module arc.syntax.tests.parser;

import arc.data.ast;
import arc.data.source: Span;
import arc.reporter;
import arc.syntax.lexer: Token;
import arc.syntax.parser: Parser, parse_expression, parse_statement, parse_type_expr;

// Thread-local module variables
Reporter reporter;
Parser parser;

static this() {
    parser = new Parser(&reporter);
}

auto parse(string op)(string text) {
    parser.reset(text);
    reporter.clear();

    mixin("return parse_" ~ op ~ "(parser);");
}

bool type_equivalent(AstNode tree, AstNode.Kind[] types...) {
    import std.algorithm: equal;

    AstNode.Kind[] flattened_tree;

    void flatten(AstNode n) {
        if (!n) return; // To accomodate for development where a parse fn returns null.
        flattened_tree ~= n.kind;
        foreach (child; n.children)
            flatten(child);
    }

    flatten(tree);

    return equal(flattened_tree, types);
}

bool check_types(AstNode node, AstNode.Kind[] types...) {
    const had_errors = reporter.errors.length > 0;
    if (had_errors) reporter.clear();

    return !had_errors && type_equivalent(node, types);
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

@("parse typedecl") unittest {
    with (AstNode.Kind) {
        {
            auto type = "def T : T2;".parse!"statement"();
            assert(type.span == Span(0, 11));
            assert(check_types(type,
                TypeDeclaration,
                    Name,
                    Name
            ));
        }

        {
            auto type = "def T : (u32, k: u32);".parse!"statement"();
            assert(type.span == Span(0, 22));
            assert(check_types(type,
                TypeDeclaration,
                    Name,
                    List,
                        Variable,
                            None,
                            Name,
                            Inferred,
                        Variable,
                            Name,
                            Name,
                            Inferred
            ));
        }

        {
            auto error = "def T : (!);".parse!"statement"();
            assert(error.span == Span(0, 12));
            assert(type_equivalent(error,
                TypeDeclaration,
                    Name,
                    Invalid
            ));
        }
    }
}

@("parse constdecl") unittest {
    with (AstNode.Kind) {
        {
            auto decl = "def T := 1;".parse!"statement"();
            assert(check_types(decl,
                ConstantDeclaration,
                    Name,
                    Inferred,
                    Integer,
            ));
        }

        {
            auto decl = "def T : a = b;".parse!"statement"();
            assert(check_types(decl,
                ConstantDeclaration,
                    Name,
                    Name,
                    Name,
            ));
        }

        {
            auto error = "def T := +;".parse!"statement"();
            assert(error.span == Span(0, 11));
            assert(type_equivalent(error,
                ConstantDeclaration,
                    Name,
                    Inferred,
                    Invalid
            ));
        }
    }
}

@("parse vardecl") unittest {
    with (AstNode.Kind) {
        {
            auto var = "a : T = 3;".parse!"statement"();
            assert(var.span == Span(0, 9));
            assert(check_types(var,
                Variable,
                    Name,
                    Name,
                    Integer
            ));
        }

        {
            auto var = "a := 3;".parse!"statement"();
            assert(var.span == Span(0, 6));
            assert(check_types(var,
                Variable,
                    Name,
                    Inferred,
                    Integer
            ));
        }

        {
            auto var = "a : T;".parse!"statement"();
            assert(var.span == Span(0, 5));
            assert(check_types(var,
                Variable,
                    Name,
                    Name,
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

@("parse name") unittest {
    assert(check_types("a_name".parse!"expression", AstNode.Kind.Name));
}

@("parse int") unittest {
    assert(check_types("20".parse!"expression", AstNode.Kind.Integer));
}

@("parse char") unittest {
    assert(check_types("'a'".parse!"expression", AstNode.Kind.Char));
}

@("parse unary") unittest {
    with (AstNode.Kind) {
        assert(check_types("-var".parse!"expression", Unary, Name, Name));
    }
}

@("parse binary") unittest {
    with (AstNode.Kind) {
        assert(check_types("1 + 1".parse!"expression", Binary, Name, Integer, Integer));
        assert(check_types("a ^ 2".parse!"expression", Binary, Name, Name, Integer));

        assert(check_types("a - -3".parse!"expression",
            Binary,
                Name,
                Name,
                Unary,
                    Name,
                    Integer
        ));

        assert(check_types("b - c + d * e ^ f ^ g / h".parse!"expression",
            Binary,
                Name,                       // +
                Binary,
                    Name,                   // -
                    Name,                   // b
                    Name,                   // c
                Binary,
                    Name,                   // /
                    Binary,
                        Name,               // *
                        Name,               // d
                        Binary,
                            Name,           // ^
                            Name,           // e
                            Binary,
                                Name,       // ^
                                Name,       // f
                                Name,       // g
                    Name                    // h
        ));
    }
}

@("parse call") unittest {
    with (AstNode.Kind) {
        assert(check_types("a()".parse!"expression"(), Call, Name, List));
        assert(check_types("a * b()".parse!"expression"(),
            Binary,
                Name,
                Name,
                Call,
                    Name,
                    List
        ));
    }
}

@("parse list") unittest {
    with (AstNode.Kind) {
        {
            auto list = "()".parse!"expression"();
            assert(list.span == Span(0, 2));
            assert(check_types(list, List));
        }

        {
            auto list = "(a:= 2, 3, c:T=blah)".parse!"expression"();
            assert(list.span == Span(0, 20));
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
                        Name
            ));
        }

        {
            auto err = "(a = b)".parse!"expression"();
            assert(reporter.has_error(ArcError.TokenExpectMismatch));
            assert(reporter.errors.length == 1);
            check_types(err, Invalid);
        }
    }
}

@("parse errors") unittest {
    with (AstNode.Kind) {
        {
            auto err = "a = = b".parse!"expression"();
            assert(err.kind == Invalid);
            assert(reporter.errors[0].code == ArcError.TokenNotAnExpression);
        }
    }
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

@("parse types") unittest {
    with (AstNode.Kind) {
        assert(check_types("a".parse!"type_expr"(), Name));

        assert(check_types("a.b".parse!"type_expr"(), Binary, Name, Name, Name));

        assert(check_types("(a)".parse!"type_expr"(), List, Variable, None, Name, Inferred));

        assert(check_types("(a:b)".parse!"type_expr"(), List, Variable, Name, Name, Inferred));

        assert(check_types("(a := 3)".parse!"type_expr"(), List, Variable, Name, Inferred, Integer));
    }
}

@("parse bad types") unittest {
    with (AstNode.Kind) {
        {
            auto err = "(a = b)".parse!"type_expr"();
            assert(reporter.has_error(ArcError.TokenExpectMismatch));
            assert(reporter.errors.length == 1);
            check_types(err, Invalid);
        }

        {
            auto err = "(a : !)".parse!"type_expr"();
            assert(reporter.has_error(ArcError.TokenExpectMismatch));
            assert(reporter.errors.length == 1);
            check_types(err, Invalid);
        }
    }
}
