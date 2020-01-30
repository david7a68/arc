module arc.syntax.tests.parser;

import arc.syntax.parser;
import arc.syntax.lexer: Lexer, Token;
import arc.syntax.ast: AstNode;
import arc.syntax.reporter: SyntaxReporter, SyntaxError;

struct ParseResult {
    AstNode tree;
    SyntaxReporter errors;
}

/// Parses a statement.
/// Info: Don't forget, expressions are statements too!
auto parse(string category)(const(char)[] text) {
    import arc.syntax.location: Source, SpannedText;

    auto span = SpannedText(0, cast(uint) text.length, text);
    auto p = ParseCtx(
        Lexer(span),
        SyntaxReporter(Source("test", span), true)
    );

    static if (category == "statement")
        p.tokens.push_eol_delimiter(Token.Semicolon);

    p.tokens.advance();
    mixin("return ParseResult(parse_" ~ category ~ "(p), p.errors);");
}

bool type_equivalent(ParseResult result, AstNode.Type[] types...) {
    import std.stdio: writefln;

    AstNode.Type[] flattened_tree;

    void flatten(AstNode n) {
        if (!n) return; // To accomodate for development where a parse fn returns null.
        flattened_tree ~= n.type;
        foreach (child; n.get_children())
            flatten(child);
    }

    flatten(result.tree);

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

bool check_error(ParseResult result, SyntaxError error, AstNode.Type[] types...) {
    return result.errors.has_error(error) && type_equivalent(result, types);
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

@("parse:def") unittest {
    with (AstNode.Type)
    assert(type_equivalent("def a := 3".parse!"statement",
        Define,
            Name,
            InferredType,
            Integer
    ));

    with (AstNode.Type)
    assert(type_equivalent("def T :\n(int\nint)".parse!"statement",
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
    assert(type_equivalent("def f : F = (a) -> a".parse!"statement",
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

@("parse:if") unittest {
    assert(type_equivalent("if a {}".parse!"statement", AstNode.If, AstNode.Name, AstNode.Block, AstNode.None));

    assert(type_equivalent("if a {} else b;".parse!"statement", AstNode.If, AstNode.Name, AstNode.Block, AstNode.Name));

    with (AstNode.Type)
    assert(type_equivalent("if a b; else c;".parse!"statement",
        If,
            Name,
            Name,
            Name
    ));

    with (AstNode.Type)
    assert(type_equivalent("if a b; else if c d; else e;".parse!"statement",
        If,
            Name,
            Name,
            If,
                Name,
                Name,
                Name
    ));
    
    with (AstNode.Type)
    assert(type_equivalent("if a b;\nelse c;".parse!"statement",
        If,
            Name,
            Name,
            Name
    ));
}

@("parse:break") unittest {
    assert(type_equivalent("break".parse!"statement", AstNode.Break));
}

@("parse:return") unittest {
    assert(type_equivalent("return".parse!"statement", AstNode.Return, AstNode.None));

    assert(type_equivalent("return a".parse!"statement", AstNode.Return, AstNode.Name));

    assert(type_equivalent("{return}".parse!"statement", AstNode.Block, AstNode.Return, AstNode.None));
}

@("parse:continue") unittest {
    assert(type_equivalent("continue".parse!"statement", AstNode.Continue));
}

@("parse:loop") unittest {
    assert(type_equivalent("loop {}".parse!"statement", AstNode.Loop, AstNode.Block));
    
    with (AstNode.Type)
    assert(type_equivalent("loop a = a + 1".parse!"statement", Loop, Assign, Name, Add, Name, Integer));
}

@("parse:assign") unittest {
    assert(type_equivalent("a = 3".parse!"statement", AstNode.Assign, AstNode.Name, AstNode.Integer));
}

@("parse:variable") unittest {
    with (AstNode.Type)
    assert(type_equivalent("a : = b".parse!"statement",
        Variable,
            Name,
            InferredType,
            Name
    ));

    with (AstNode.Type)
    assert(type_equivalent("a : b".parse!"statement",
        Variable,
            Name,
            Name,
            None,
    ));

    with (AstNode.Type)
    assert(type_equivalent("a : b = c".parse!"statement",
            Variable,
                Name,
                Name,
                Name,
    ));
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

@("parse:name") unittest {
    assert(type_equivalent("a_name".parse!"expression", AstNode.Name));
}

@("parse:int") unittest {
    assert(type_equivalent("20".parse!"expression", AstNode.Integer));
}

@("parse:char") unittest {
    assert(type_equivalent("'a'".parse!"expression", AstNode.Char));
}

@("parse:unary") unittest {
    with (AstNode.Type) {
        assert(type_equivalent("-var".parse!"expression", Negate, Name));
        assert(type_equivalent("*int".parse!"expression", Pointer, Name));
        assert(type_equivalent("&int".parse!"expression", GetRef, Name));
        assert(type_equivalent("&a.b".parse!"expression", GetRef, Call, Name, Name));
    }
}

@("parse:binary") unittest {
    assert(type_equivalent("a + 1".parse!"expression", AstNode.Add, AstNode.Name, AstNode.Integer));

    assert(type_equivalent("a++".parse!"expression", AstNode.Invalid));

    assert(type_equivalent("foo()".parse!"expression", AstNode.Call, AstNode.Name, AstNode.List));

    assert(type_equivalent("a.b".parse!"expression", AstNode.Call, AstNode.Name, AstNode.Name));

    with (AstNode.Type)
    assert(type_equivalent("(foo()())()".parse!"expression",
        Call,
            List,
                ListMember,
                    None,
                    InferredType,
                    Call,
                        Call,
                            Name,
                            List,
                        List,
            List,
    ));

    with (AstNode.Type)
    assert(type_equivalent("b - c + d * e ^ f ^ g / h".parse!"expression",
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

    with (AstNode.Type)
    assert(type_equivalent("(a) -> {b}()".parse!"expression",
        Call,
            Function,
                List,
                    ListMember,
                        None,
                        InferredType,
                        Name,
                InferredType,
                Block,
                    Name,
            List,
    ));
}

@("parse:list") unittest {
    with (AstNode.Type) {
        assert(type_equivalent("()".parse!"expression", List));
        
        assert(type_equivalent("(,0, b:b1, c = 3, d : d1 = 4,)".parse!"expression",
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

        assert(type_equivalent("[0, b:b1, c = 3, d : d1 = 4]".parse!"expression",
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

@("parse:bad_list") unittest {
    with (AstNode.Type) {
        assert(check_error("(".parse!"expression", SyntaxError.UnexpectedEndOfFile, Invalid));
        assert(check_error("(a=".parse!"expression", SyntaxError.UnexpectedEndOfFile, Invalid));
        assert(check_error("(a=)".parse!"expression", SyntaxError.TokenNotAnExpression, List, Invalid));
        assert(check_error("(a:=2)".parse!"expression", SyntaxError.TokenNotAnExpression, List, Invalid));
        assert(check_error("(a++, 2)".parse!"expression", SyntaxError.TokenNotAnExpression, List, Invalid, ListMember, None, InferredType, Integer));
    }
}

@("parse:block") unittest {
    assert(type_equivalent("{}".parse!"expression", AstNode.Block));
}

@("parse:function") unittest {
    with (AstNode.Type) {
        assert(type_equivalent("() -> 1".parse!"expression",
            Function,
                List,               // There are no parameters in this function.
                InferredType,       // The return type of the function is derived from the function body.
                Integer             // The function's expression body.
        ));

        assert(type_equivalent("(a, b:c) -> a".parse!"expression",
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

        assert(type_equivalent("() -> { blah() }".parse!"expression",
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

@("parse:type_name") unittest {
    assert(type_equivalent("T".parse!"type", AstNode.Name));
}

@("parse:pointer_type") unittest {
    assert(type_equivalent("*Y".parse!"type", AstNode.PointerType, AstNode.Name));
}

@("parse:type_list") unittest {
    with (AstNode.Type) {
        assert(type_equivalent("(int, named: int)".parse!"type",
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

@("parse:call_result_type") unittest {
    with (AstNode.Type)
    assert(type_equivalent("a.b()".parse!"type",
        Call,
            Call,
                Name,
                Name,
            List,
    ));
}

@("parse:function_types") unittest {
    with (AstNode.Type) {
        assert(type_equivalent("() -> T".parse!"type",
            FunctionType,
                TypeList,           // There are no parameters for this function.
                Name,               // The return type.
        ));

        assert(type_equivalent("() -> A.B()".parse!"type",
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
