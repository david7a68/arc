module arc.syntax.tests.parser;

import arc.syntax.ast: AstNode;
import arc.syntax.reporter: SyntaxReporter;
import arc.syntax.parser;

struct ParseResult {
    AstNode* tree;
    SyntaxReporter errors;
    uint count;
}

ParseResult parse(alias parse_fn)(const(char)[] text) {
    import arc.stringtable: StringTable;
    import arc.syntax.location: SourceMap;

    static StringTable table;
    SourceMap sources;
    
    auto source = sources.put("", text);
    auto error = new SyntaxReporter(source);
    auto parser = Parser(source.span, &table, error);
    
    return ParseResult(parse_fn(parser), error, parser.count);
}

AstNode.Type[] check_tree(AstNode* tree, AstNode.Type[] types) {
    bool check_subtree(AstNode* subtree, ref AstNode.Type[] types) {
        if (subtree is null)
            return types.length == 0;
        
        if (types.length == 0 || subtree.type != types[0])
            return false;
        
        types = types[1..$];
        foreach (i; 0 .. subtree.num_children) {
            const passed = check_subtree(subtree.children[i], types);
            if (!passed) return false;
        }

        return true;
    }

    check_subtree(tree, types);
    return types;
}

AstNode.Type[] test_parse(alias fn)(const(char)[] text, AstNode.Type[] types) {
    return check_tree(parse!fn(text).tree, types);
}

alias test_statement = test_parse!parse_statement;
alias test_type = test_parse!parse_type;
alias test_expression = test_parse!parse_expression;

@("parser:def") unittest {
    with (AstNode.Type) {
        assert(test_statement("def a:\nT=\ninit;", [
            Define,
                Name,
                Name,
                Name,
        ]) == []);

        assert(test_statement("def a: [b:\nT\n\nc]", [
            Define,
                Name,
                TypeList,
                    TypeListMember,
                        Name,
                        Name,
                    TypeListMember,
                        None,
                        Name,
                None,
        ]) == []);

        assert(test_statement("def a:\n=4", [
            Define,
                Name,
                None,
                Integer
        ]) == []);
    }
}

@("parser:break") unittest {
    with (AstNode.Type) {
        assert(test_statement("break;", [
            Break,
                None,
                None,
        ]) == []);

        assert(test_statement("break 'here\n;", [
            Break,
                Label,
                None
        ]) == []);

        assert(test_statement("break -a *\n b;", [
            Break,
                None,
                Multiply,
                    Negate,
                        Name,
                    Name
        ]) == []);

        assert(test_statement("break 'here a;", [
            Break,
                Label,
                Name
        ]) == []);

        assert(test_statement("{break}", [
            Block,
                Break,
                    None,
                    None
        ]) == []);
    }
}

@("parser:return") unittest {
    with (AstNode.Type) {
        assert(test_statement("return;", [
            Return,
                None,
                None,
        ]) == []);

        assert(test_statement("return 'here;", [
            Return,
                Label,
                None
        ]) == []);

        assert(test_statement("return -a * b;", [
            Return,
                None,
                Multiply,
                    Negate,
                        Name,
                    Name
        ]) == []);

        assert(test_statement("return 'here a;", [
            Return,
                Label,
                Name
        ]) == []);

        assert(test_statement("{return}", [
            Block,
                Return,
                    None,
                    None
        ]) == []);
    }
}

@("parser:type") unittest {
    with (AstNode.Type) {
        assert(test_type("(a) -> b", [
            FunctionType,
                TypeList,
                    TypeListMember,
                        None,
                        Name,
                Name,
        ]) == []);

        assert(test_type("(a:b, c:d) -> e", [
            FunctionType,
                TypeList,
                    TypeListMember,
                        Name,
                        Name,
                    TypeListMember,
                        Name,
                        Name,
                Name
        ]) == []);

        assert(test_type("()", [TypeList]) == []);
    }
}

@("parser:label") unittest {
    with (AstNode.Type) {
        assert(test_expression("'any_name9", [
            Label
        ]) == []);

        assert(test_expression("'l loop {}", [
            Labeled,
                Label,
                    Loop,
                        Block
        ]) == []);

        assert(test_expression("'l if e {break 'l}", [
            Labeled,
                Label,
                If,
                    Name,
                    Block,
                        Break,
                            Label,
                            None,
                    None
        ]) == []);

        assert(test_expression("'l (a) -> { return 'l a * a }", [
            Labeled,
                Label,
                Function,
                    List,
                        ListMember,
                            None,
                            None,
                            Name,
                    None,
                    Block,
                        Return,
                            Label,
                            Multiply,
                                Name,
                                Name
        ]) == []);
    }
}

@("parser:var_expr") unittest {
    with (AstNode.Type) {
        assert(test_expression("a : b()", [
            VarExpression,
                Name,
                Call,
                    Name,
                    List,
            None
        ]) == []);

        assert(test_expression("a:b=c", [
            VarExpression,
                Name,
                Name,
                Name,
        ]) == []);
    }
}

@("parser:name") unittest {
    assert(test_expression("aks_1", [AstNode.Name]) == []);
}

@("parser:int") unittest {
    assert(test_expression("1019", [AstNode.Integer]) == []);
}

@("parser:char") unittest {
    assert(test_expression("'a'", [AstNode.Char]) == []);
    assert(test_expression("'\\n'", [AstNode.Char]) == []);
}

@("parser:list") unittest {
    with (AstNode.Type)
    assert(test_expression("[,a, a:t, b=(4), a:b()=c]", [
        List,
            ListMember,
                None,
                None,
                Name,
            ListMember,
                Name,
                Name,
                None,
            ListMember,
                Name,
                None,
                List,
                    ListMember,
                        None,
                        None,
                        Integer,
            ListMember,
                Name,
                Call,
                    Name,
                    List,
                Name
    ]) == []);
}

@("parser:block") unittest {
    with (AstNode.Type)
    assert(test_expression("{ a:= 3\n return a * 2 }", [
        Block,
            VarExpression,
                Name,
                None,
                Integer,
            Return,
                None,
                Multiply,
                    Name,
                    Integer
    ]) == []);
}

@("parser:function") unittest {
    with (AstNode.Type) {
        assert(test_expression("(a) -> b * c", [
            Function,
                List,
                    ListMember,
                        None,
                        None,
                        Name,
                None,
                Multiply,
                    Name,
                    Name,
        ]) == []);

        assert(test_expression("(a) -> r {}", [
            Function,
                List,
                    ListMember,
                        None,
                        None,
                        Name,
                Name,
                Block
        ]) == []);
    }
}

@("parser:call") unittest {
    with (AstNode.Type)
    assert(test_expression("[][()]", [
        Call,
            List,
            List,
                ListMember,
                    None,
                    None,
                    List,
    ]) == []);
}

@("parser:dot") unittest {
    with (AstNode.Type)
    assert(test_expression("a().c", [
        Call,
            Call,
                Name,
                List,
            Name
    ]) == []);
}

@("parser:unary") unittest {
    with (AstNode.Type) {
        assert(test_expression("-(a.b)()", [
            Negate,
                Call,
                    List,
                        ListMember,
                            None,
                            None,
                            Call,
                                Name,
                                Name,
                    List,
        ]) == []);

        assert(test_expression("*a", [
            Pointer,
                Name
        ]) == []);

        assert(test_expression("&a.b.c", [
            GetRef,
                Call,
                    Call,
                        Name,
                        Name,
                    Name
        ]) == []);
    }
}

@("parser:binary") unittest {
    with (AstNode.Type)
    assert(test_expression("a = b - c + d * e ^ f ^ g / h", [
        Assign,
            Name,
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
    ]) == []);
}

@("parser:if") unittest {
    with (AstNode.Type) {
        assert(test_expression("if a {} else {}", [
            If,
                Name,
                Block,
                Block
        ]) == []);

        assert(test_expression("if a {} else if b {}", [
            If,
                Name,
                Block,
                If,
                    Name,
                    Block,
                    None
        ]) == []);
    }
}

@("parser:loop") unittest {
    with (AstNode.Type) {
        assert(test_expression("loop \n{}", [
            Loop,
                Block,
        ]) == []);
    }
}
