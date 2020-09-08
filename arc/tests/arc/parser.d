module tests.arc.parser;

import arc.analysis.lexer;
import arc.analysis.parser;
import arc.data.ast;
import arc.data.scopes;
import arc.data.stringtable;
import arc.data.symbol;
import arc.reporter;
import std.stdio : writefln;

@("Parsing and AST Construction") unittest {
    auto symbols = new GlobalSymbolTable(64);
    auto errors = Reporter();
    auto strings = StringTable();
    auto parser = new Parser(
        &errors,
        &strings,
        new AstAllocator(),
        new ScopeAllocator(symbols),
        symbols,
    );

    value_literals(errors, parser);

    simple_prefix_expressions(errors, parser);
    
    simple_infix_expressions(errors, parser);
    
    simple_list_expressions(errors, parser);

    function_literals(errors, parser);

    complex_expressions(errors, parser);

    simple_statements(errors, parser);
}

void value_literals(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.expr(`a`), SymbolRef));
        assert(check_types(errors, parser.expr(`2`), Integer));
        assert(check_types(errors, parser.expr(`'a'`), Char));
        assert(check_types(errors, parser.expr(`"a"`), String));
    }
}

void simple_prefix_expressions(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.expr(`-1`), Negate, Integer));
        assert(check_types(errors, parser.expr(`not a`), Not, SymbolRef));
        assert(check_types(errors, parser.expr(`!a`), Not, SymbolRef));
        assert(check_types(errors, parser.expr(`*a`), Dereference, SymbolRef));
        assert(check_types(errors, parser.expr(`import a`), Import, SymbolRef));
    }
}

void simple_infix_expressions(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.expr(`a<b`), Less, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a<=b`), LessEqual, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a>b`), Greater, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a>=b`), GreaterEqual, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a==b`), Equal, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a!=b`), NotEqual, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a and b`), And, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a or b`), Or, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a+b`), Add, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a-b`), Subtract, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a*b`), Multiply, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a/b`), Divide, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a^b`), Power, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a.b`), Access, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a::b`), StaticAccess, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a()`), Call, SymbolRef, List));
        assert(check_types(errors, parser.expr(`a[]`), Call, SymbolRef, List));
        assert(check_types(errors, parser.expr(`a b`), Call, SymbolRef, SymbolRef));
        assert(check_types(errors, parser.expr(`a 1`), Call, SymbolRef, Integer));
        assert(check_types(errors, parser.expr(`a 'a'`), Call, SymbolRef, Char));
        assert(check_types(errors, parser.expr(`a "b"`), Call, SymbolRef, String));

        // Testing Precedence Parsing in Increasing Order
        assert(check_types(errors, parser.expr(`1 < a and b == 2`),
            And,
                Less,
                    Integer,
                    SymbolRef,
                Equal,
                    SymbolRef,
                    Integer));
        
        assert(check_types(errors, parser.expr(`a < 1 == true`),
            Equal,
                Less,
                    SymbolRef,
                    Integer,
                SymbolRef));
        
        assert(check_types(errors, parser.expr(`1 + 2 < a - -b`),
            Less,
                Add,
                    Integer,
                    Integer,
                Subtract,
                    SymbolRef,
                    Negate,
                        SymbolRef));
        
        assert(check_types(errors, parser.expr(`-2 - b`),
            Subtract,
                Negate,
                    Integer,
                SymbolRef));

        assert(check_types(errors, parser.expr(`*a.b`),
            Dereference,
                Access,
                    SymbolRef,
                    SymbolRef));

        assert(check_types(errors, parser.expr(`2 * x ^ 3 + 3 / y - 1`),
            Subtract,                   // 2 * x ^ 3 + 3 / y - 1
                Add,                    // 2 * x ^ 3 + 3 / y
                    Multiply,           // 2 * x ^ 3
                        Integer,
                        Power,          // x ^ 3
                            SymbolRef,
                            Integer,
                    Divide,             // 3 / y
                        Integer,
                        SymbolRef,
                Integer));
        
        assert(check_types(errors, parser.expr(`a.b ^ c::d`),
            Power,
                Access,
                    SymbolRef,
                    SymbolRef,
                StaticAccess,
                    SymbolRef,
                    SymbolRef));

        assert(check_types(errors, parser.expr(`a.b::c()`),
            Call,
                StaticAccess,
                    Access,
                        SymbolRef,
                        SymbolRef,
                    SymbolRef,
                List));

        assert(check_types(errors, parser.expr(`a b 2`),
            Call,
                SymbolRef,
                Call,
                    SymbolRef,
                    Integer));
        
        assert(check_types(errors, parser.expr(`a() 'b'`),
            Call,                       // a() 'b'
                Call,                   // a()
                    SymbolRef,
                    List,
                Char));
        
        assert(check_types(errors, parser.expr(`a() b()`),
            Call,
                Call,
                    SymbolRef,
                    List,
                Call,
                    SymbolRef,
                    List));
    }
}

void simple_list_expressions(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.expr(`()`), List));
        assert(check_types(errors, parser.expr(`[]`), List));
        assert(check_types(errors, parser.expr(`(a)`), List, ListMember, InferredType, SymbolRef));
        
        assert(check_types(errors, parser.expr(`[a:=1, b:R, c:T=d]`),
            List,
                ListMember,
                    InferredType,
                    Integer,
                ListMember,
                    SymbolRef,
                    None,
                ListMember,
                    SymbolRef,
                    SymbolRef
        ));

        assert(check_types(errors, parser.expr(`(((())))`),
            List,
                ListMember,
                    InferredType,
                    List,
                        ListMember,
                            InferredType,
                            List,
                                ListMember,
                                    InferredType,
                                    List
        ));

        assert(check_error(errors, parser.expr(`(a=b)`), ArcError.TokenExpectMismatch));
        assert(check_error(errors, parser.expr(`(!)`), ArcError.TokenExpectMismatch));
    }
}

void function_literals(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.expr(`(a) -> b`),
            FunctionSignature,
                List,
                    ListMember,
                        InferredType,
                        SymbolRef,
                SymbolRef));
        
        assert(check_types(errors, parser.expr(`(a) => b`),
            Function,
                List,
                    ListMember,
                        InferredType,
                        SymbolRef,
                InferredType,
                SymbolRef));
        

        assert(check_types(errors, parser.expr(`() => {}`),
            Function,
                List,
                InferredType,
                Block));

        assert(check_types(errors, parser.expr(`() => T {}`),
            Function,
                List,
                SymbolRef,
                Block));
        
        assert(check_types(errors, parser.expr(`(() -> T)`),
            List,
                ListMember,
                    InferredType,
                    FunctionSignature,
                        List,
                        SymbolRef));
        
        assert(check_types(errors, parser.expr(`(T) -> () -> tf()`),
            FunctionSignature,      // (T) -> () -> tf()
                List,               // (T)
                    ListMember,
                        InferredType,
                        SymbolRef,
                FunctionSignature,  // () -> tf()
                    List,
                    Call,
                        SymbolRef,
                        List));
        
        assert(check_types(errors, parser.expr(`(() => 1)`),
            List,
                ListMember,
                    InferredType,
                    Function,
                        List,
                        InferredType,
                        Integer));
    }
}

/// More involved expressions that mix subexpression types.
void complex_expressions(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.expr(`(import a)::c`),
            StaticAccess,
                List,
                    ListMember,
                        InferredType,
                        Import,
                            SymbolRef,
                SymbolRef));
    }
}

void simple_statements(ref Reporter errors, Parser parser) {
    with (AstNode.Kind) {
        assert(check_types(errors, parser.stmt(`{}`), Block));
        assert(check_types(errors, parser.stmt(`loop {}`), Loop));
        assert(check_types(errors, parser.stmt(`break;`), Break));
        assert(check_types(errors, parser.stmt(`continue;`), Continue));
        assert(check_types(errors, parser.stmt(`return;`), Return, None));
        assert(check_types(errors, parser.stmt(`return 1;`), Return, Integer));

        assert(check_types(errors, parser.stmt(`if 1 {}`),
            If,
                Integer,
                Block,
                None));
        
        assert(check_types(errors, parser.stmt(`if a and not b {} else {}`),
            If,
                And,
                    SymbolRef,
                    Not,
                        SymbolRef,
                Block,
                Block));

        assert(check_types(errors, parser.stmt(`def a := 1;`),
            Definition,
                InferredType,
                Integer));

        assert(check_types(errors, parser.stmt(`def a : () -> T = () => 3;`),
            Definition,
                FunctionSignature,
                    List,
                    SymbolRef,
                Function,
                    List,
                    InferredType,
                    Integer));

        assert(check_types(errors, parser.stmt(`a := 3;`), Variable, InferredType, Integer));
        assert(check_types(errors, parser.stmt(`a : T[] = 3;`),
            Variable,
                Call,
                    SymbolRef,
                    List,
                Integer));

        assert(check_types(errors, parser.stmt(`a = 3;`), Assign, SymbolRef, Integer));

        assert(check_types(errors, parser.stmt(`{break;}`), Block, Break));
        assert(check_types(errors, parser.stmt(`{continue;}`), Block, Continue));
        assert(check_types(errors, parser.stmt(`{return;}`), Block, Return, None));
        assert(check_types(errors, parser.stmt(`{return 'a';}`), Block, Return, Char));

        assert(check_types(errors, parser.stmt(`loop {break;}`), Loop, Break));
        assert(check_types(errors, parser.stmt(`loop {continue;}`), Loop, Continue));
        assert(check_types(errors, parser.stmt(`loop {return;}`), Loop, Return, None));
        assert(check_types(errors, parser.stmt(`loop {return 'a';}`), Loop, Return, Char));

        assert(check_types(errors, parser.stmt(`a;`), SymbolRef));
        assert(check_types(errors, parser.stmt(`1 + 1;`), Add, Integer, Integer));
    }
}

alias stmt = parse!"stmt";
alias expr = parse!"expr";

SyntaxTree parse(string op)(Parser parser, string text) {
    import arc.source : Source;

    // IMPORTANT!!! WE DELIBERATELY ESCAPE THIS POINTER.
    // DO NOT USE SyntaxTree.source !!!!!
    auto source = Source("", text, 0);

    mixin("return parser.parse_" ~ op ~ "(&source);");
}

bool check_types(ref Reporter reporter, SyntaxTree result, AstNode.Kind[] expected...) {
    if (reporter.errors.length) {
        writefln(
            "Test Failed: Parsing errors detected when none were expected.\n" ~
            "Source: %s\nErrors: %-(%s%)\n",
            result.source, reporter.errors);

        return false;
    }
    return type_equivalent(result, expected);
}

bool check_error(ref Reporter reporter, SyntaxTree result, ArcError.Code error) {
    scope (exit) reporter.clear();

    if (!type_equivalent(result, AstNode.Kind.Invalid))
        return false;

    if (!reporter.has_error(error)) {
        writefln(
            "Test Failed: The parser did not encounter the expected error.\n" ~
            "Source: %s\n" ~
            "Expected Error Code: %s\nEncountered Errors: %s\n",
            result.source, error, reporter.errors);
        return false;
    }
    return true;
}

bool type_equivalent(SyntaxTree tree, AstNode.Kind[] expected...) {
    import std.algorithm : equal;

    AstNode.Kind[] tree_types;
    void flatten(AstNodeId[] nodes) {
        foreach (n; nodes) {
            tree_types ~= tree[n].kind;
            flatten(tree.children_of(n));
        }
    }

    flatten(tree.statements);

    if (equal(expected, tree_types))
        return true;

    writefln(
            "Test Failed: The parsed AST is not as expected.\n" ~
            "Source: %s\n" ~
            "Produced AST Types:\n\t%s\n" ~
            "Expected AST Types:\n\t%s\n",
            tree.source, tree_types, expected);
    return false;
}
