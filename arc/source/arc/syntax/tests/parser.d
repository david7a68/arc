module arc.syntax.tests.parser;

import arc.syntax.parser;
import arc.syntax.lexer: Token;
import arc.syntax.ast: AstNode;
import arc.syntax.reporting: SyntaxError;

struct ParseResult {
    AstNode tree;
    SyntaxError[] errors;
}

/// Parses a statement.
/// Info: Don't forget, expressions are statements too!
auto parse(string category)(const(char)[] text) {
    auto p = ParseCtx(text, 0);

    static if (category == "statement")
        p.delimiter_stack.insertBack(Token.Semicolon);

    mixin("return ParseResult(parse_" ~ category ~ "(p), p.errors);");
}

bool type_equivalent(AstNode tree, AstNode.Type[] types...) {
    import std.stdio: writefln;

    AstNode.Type[] flattened_tree;

    void flatten(AstNode n) {
        if (!n) return; // To accomodate for development where a parse fn returns null.
        flattened_tree ~= n.type;
        foreach (child; n.get_children())
            flatten(child);
    }

    flatten(tree);
    import std.stdio; writeln(flattened_tree);

    if (flattened_tree.length != types.length) {
        writefln(
            "The number of nodes (%s) in the tree and the number of types (%s) do not match!",
            flattened_tree.length,
            types.length
        );
        return false;
    }

    foreach (i, type; types) {
        if (type != flattened_tree[i]) {
            writefln("The types do not match at Tree (%s) and List (%s)", flattened_tree[i], types[i]);
            return false;
        }
    }

    return true;
}

bool check_types(ParseResult result, AstNode.Type[] types...) {
    return result.errors.length == 0 && type_equivalent(result.tree, types);
}

bool check_error(ParseResult result, SyntaxError.Code error_code, AstNode.Type[] types...) {
    bool has_error;
    foreach (error; result.errors)
        if (error.code == error_code) {
            has_error = true;
            break;
        }
    
    return has_error && type_equivalent(result.tree, types);
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

@("parse def") unittest {
    with (AstNode.Type)
    assert(check_types("def a := 3".parse!"statement",
        Define,
            Name,
            InferredType,
            Integer
    ));

    with (AstNode.Type)
    assert(check_types("def T :\n(int\nint)".parse!"statement",
        Define,
            Name,
            TypeList,
                TypeListMember,
                    None,
                    Name,
                TypeListMember,
                    None,
                    Name,
            None
    ));

    with (AstNode.Type)
    assert(check_types("def f : F = (a) -> a".parse!"statement",
        Define,
            Name,
            Name,
            Function,
                List,
                    ListMember,
                        None,
                        InferredType,
                        Name,
                InferredType,
                Name
    ));
}

@("parse if") unittest {
    with (AstNode.Type) {
        assert(check_types("if a {}".parse!"statement", If, Name, Block, None));

        assert(check_types("if a {} else {}".parse!"statement", If, Name, Block, Block));

        assert(check_types("if a {} else {}".parse!"statement",
            If,
                Name,
                Block,
                Block
        ));

        assert(check_types("if a {} else if c {} else {}".parse!"statement",
            If,
                Name,
                Block,
                If,
                    Name,
                    Block,
                    Block
        ));
        
        assert(check_types("if a {}\nelse {}".parse!"statement",
            If,
                Name,
                Block,
                Block
        ));
    }
}

@("bad if") unittest {
    assert(check_types("if a \n {}".parse!"statement", AstNode.If, AstNode.Name, AstNode.Block, AstNode.None));
}

@("parse break") unittest {
    assert(check_types("break".parse!"statement", AstNode.Break));
}

@("parse return") unittest {
    assert(check_types("return".parse!"statement", AstNode.Return, AstNode.None));

    assert(check_types("return a".parse!"statement", AstNode.Return, AstNode.Name));

    assert(check_types("{return}".parse!"statement", AstNode.Block, AstNode.Return, AstNode.None));
}

@("parse continue") unittest {
    assert(check_types("continue".parse!"statement", AstNode.Continue));
}

@("parse loop") unittest {
    assert(check_types("loop {}".parse!"statement", AstNode.Loop, AstNode.Block));
    
    with (AstNode.Type)
    assert(check_types("loop a = a + 1".parse!"statement", Loop, Assign, Name, Add, Name, Integer));
}

@("parse invalid statement") unittest {
    assert(check_error("else {}".parse!"statement", SyntaxError.UnboundElse, AstNode.Invalid));
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
    assert(check_types("a_name".parse!"expression", AstNode.Name));
}

@("parse int") unittest {
    assert(check_types("20".parse!"expression", AstNode.Integer));
}

@("parse char") unittest {
    assert(check_types("'a'".parse!"expression", AstNode.Char));
}

@("parse unary") unittest {
    with (AstNode.Type) {
        assert(check_types("-var".parse!"expression", Negate, Name));
        assert(check_types("*int".parse!"expression", Pointer, Name));
        assert(check_types("&int".parse!"expression", GetRef, Name));
        assert(check_types("&a.b".parse!"expression", GetRef, Call, Name, Name));
    }
}

@("parse binary") unittest {
    assert(check_types("a + 1".parse!"expression", AstNode.Add, AstNode.Name, AstNode.Integer));

    assert(check_error("a++".parse!"expression", SyntaxError.TokenNotAnExpression, AstNode.Invalid));

    assert(check_types("foo()".parse!"expression", AstNode.Call, AstNode.Name, AstNode.List));

    assert(check_types("a.b".parse!"expression", AstNode.Call, AstNode.Name, AstNode.Name));

    with (AstNode.Type)
    assert(check_types("b - c + d * e ^ f ^ g / h".parse!"expression",
        Add,
            Subtract,
                Name,
                Name,
            Divide,
                Multiply,
                    Name,
                    Power,
                        Name,
                        Power,
                            Name,
                            Name,
                Name
    ));

    assert(check_types("a = 3".parse!"expression", AstNode.Assign, AstNode.Name, AstNode.Integer));
}

@("parse list0") unittest {
    with (AstNode.Type) {
        assert(check_types("()".parse!"expression", List));
        
        assert(check_types("(,0, b:b1, c = 3, d : d1 = 4,)".parse!"expression",
            List,
                ListMember,
                    None,
                    InferredType,
                    Integer,
                ListMember,
                    Name,
                    Name,
                    None,
                ListMember,
                    Name,
                    InferredType,
                    Integer,
                ListMember,
                    Name,
                    Name,
                    Integer
        ));

        assert(check_types("[0, b:b1, c = 3, d : d1 = 4]".parse!"expression",
            List,
                ListMember,
                    None,
                    InferredType,
                    Integer,
                ListMember,
                    Name,
                    Name,
                    None,
                ListMember,
                    Name,
                    InferredType,
                    Integer,
                ListMember,
                    Name,
                    Name,
                    Integer
        ));
    }
}

@("parse bad_list") unittest {
    with (AstNode.Type) {
        assert(check_error("(".parse!"expression", SyntaxError.UnexpectedEndOfFile, Invalid));
        assert(check_error("(a=".parse!"expression", SyntaxError.UnexpectedEndOfFile, Invalid));
        assert(check_error("(a=)".parse!"expression", SyntaxError.TokenNotAnExpression, List, Invalid));
        assert(check_error("(a:=2)".parse!"expression", SyntaxError.TokenNotAnExpression, List, Invalid));
        assert(check_error("(a++, 2)".parse!"expression", SyntaxError.TokenNotAnExpression, List, Invalid, ListMember, None, InferredType, Integer));
    }
}

@("parse block") unittest {
    assert(check_types("{}".parse!"expression", AstNode.Block));
}

@("parse function") unittest {
    with (AstNode.Type) {
        assert(check_types("() -> 1".parse!"expression",
            Function,
                List,               // There are no parameters in this function.
                InferredType,       // The return type of the function is derived from the function body.
                Integer             // The function's expression body.
        ));

        assert(check_types("(a, b:c) -> a".parse!"expression",
            Function,
                List,                   // The parameter list.
                    ListMember,
                        None,
                        InferredType,   // The type of the member is derived from N (or it fails).
                        Name,           // 1-element list members are values. In this case, it refers to a variable
                                        // from an enclosing scope.
                    ListMember,
                        Name,           // The name of the variable of the type `c`.
                        Name,           // The type of the member is derived from N (or it fails).
                        None,
                InferredType,           // The function's return type is derived from the type of a (must match return type of N).
                Name,                   // The function's expression body.
        ));

        assert(check_types("() -> { blah() }".parse!"expression",
            Function,
                List,
                InferredType,
                Block,
                    Call,
                        Name,
                        List,
        ));
    }
}

@("parse variable") unittest {
    with (AstNode.Type)
    assert(check_types("a : = b".parse!"expression",
        Variable,
            Name,
            InferredType,
            Name
    ));

    with (AstNode.Type)
    assert(check_types("a : b".parse!"expression",
        Variable,
            Name,
            Name,
            None,
    ));

    with (AstNode.Type)
    assert(check_types("a : b = c".parse!"expression",
        Variable,
            Name,
            Name,
            Name,
    ));
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

@("parse type_name") unittest {
    assert(check_types("T".parse!"type", AstNode.Name));
}

@("parse pointer_type") unittest {
    assert(check_types("*Y".parse!"type", AstNode.PointerType, AstNode.Name));
}

@("parse type_list") unittest {
    with (AstNode.Type) {
        assert(check_types("(int, named: int)".parse!"type",
            TypeList,
                TypeListMember,
                    None,           // The member does not have a name.
                    Name,           // 1-element type list members are types.
                TypeListMember,
                    Name,           // The name of the member.
                    Name,           // The type of the member.
        ));
    }
}

@("parse call_result_type") unittest {
    with (AstNode.Type)
    assert(check_types("a.b()".parse!"type",
        Call,
            Call,
                Name,
                Name,
            List,
    ));
}

@("parse function_types") unittest {
    with (AstNode.Type) {
        assert(check_types("() -> T".parse!"type",
            FunctionType,
                TypeList,           // There are no parameters for this function.
                Name,               // The return type.
        ));

        assert(check_types("() -> A.B()".parse!"type",
            FunctionType,
                TypeList,           // There are no parameters for this function.
                Call,               // The return type is the result of a call expression.
                    Call,
                        Name,
                        Name,
                    List
        ));
    }
}
