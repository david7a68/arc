module arc.syntax.parser_tests;

version(unittest):

import arc.stringtable;
import arc.syntax.ast;
import arc.syntax.parser;
import arc.syntax.location;
import arc.syntax.reporter;

string parser_init(string test_name, string s, string parser_name, bool err_check = true) {
    import std.format: format;
    return "
        StringTable table;
        SourceMap sources;
        auto source = sources.put(\"%s\", \"%s\");
        auto error = new SyntaxReporter(source);
        auto parser = Parser(source.span, &table, error);
        auto tree = parser.parse_%s();
        assert(parser.empty);
        %s".format(test_name, s, parser_name, err_check ? "assert(parser.reporter.errors.length == 0);" : "");
}


struct Match {
    AstNode.Type node_type;
    Match[] children;
}

AstNode*[] diff(AstNode* root, Match match) {
    AstNode*[] r;
    if (root.type != match.node_type || root.num_children != match.children.length)
        r ~= root;
    else if(root.num_children > 0) {
        foreach (i, child; root.children)
            r ~= diff(child, match.children[i]);
    }
    return r;
}

// ---------- Tests Begin Here ---------- //

@("parser:def") unittest {
    mixin(parser_init("parser:def", "def a: T = init;", "statement"));
    assert(diff(tree, Match(AstNode.Define, [
        Match(AstNode.Name),
        Match(AstNode.Name),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:break") unittest {
    {
        mixin(parser_init("parser:break", "break;", "statement"));
        assert(diff(tree, Match(AstNode.Break, [
            Match(AstNode.None),
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:break", "break 'here;", "statement"));
        assert(diff(tree, Match(AstNode.Break, [
            Match(AstNode.Label),
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:break", "break -a * b;", "statement"));
        assert(diff(tree, Match(AstNode.Break, [
            Match(AstNode.None),
            Match(AstNode.Multiply, [
                Match(AstNode.Negate, [ Match(AstNode.Name) ]),
                Match(AstNode.Name)
            ]),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:break", "break 'here a;", "statement"));
        assert(diff(tree, Match(AstNode.Break, [
            Match(AstNode.Label),
            Match(AstNode.Name)
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:break", "{break}", "statement"));
        assert(diff(tree, Match(AstNode.Block, [
            Match(AstNode.Break, [
                Match(AstNode.None),
                Match(AstNode.None),
            ])
        ])).length == 0);
    }
}

@("parser:return") unittest {
    {
        mixin(parser_init("parser:return", "return;", "statement"));
        assert(diff(tree, Match(AstNode.Return, [
            Match(AstNode.None),
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:return", "return 'here;", "statement"));
        assert(diff(tree, Match(AstNode.Return, [
            Match(AstNode.Label),
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:return", "return -a * b;", "statement"));
        assert(diff(tree, Match(AstNode.Return, [
            Match(AstNode.None),
            Match(AstNode.Multiply, [
                Match(AstNode.Negate, [ Match(AstNode.Name) ]),
                Match(AstNode.Name)
            ]),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:return", "return 'here a;", "statement"));
        assert(diff(tree, Match(AstNode.Return, [
            Match(AstNode.Label),
            Match(AstNode.Name)
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:return", "{return}", "statement"));
        assert(diff(tree, Match(AstNode.Block, [
            Match(AstNode.Return, [
                Match(AstNode.None),
                Match(AstNode.None),
            ])
        ])).length == 0);
    }
}

@("parser:continue") unittest {
    {
        mixin(parser_init("parser:continue", "continue;", "statement"));
        assert(diff(tree, Match(AstNode.Continue, [
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:continue", "continue 'here;", "statement"));
        assert(diff(tree, Match(AstNode.Continue, [
            Match(AstNode.Label),
        ])).length == 0);
    }
    {
        mixin(parser_init("parser:continue", "{continue}", "statement"));
        assert(diff(tree, Match(AstNode.Block, [
            Match(AstNode.Continue, [
                Match(AstNode.None),
            ])
        ])).length == 0);
    }
}

@("parser:label") unittest {
    mixin(parser_init("parser:label", "'any_alpha_name0", "expression"));
    assert(diff(tree, Match(AstNode.Label)).length == 0);
}

@("parser:label2") unittest{
    mixin(parser_init("parser:label2", "'label *c", "expression"));
    assert(diff(tree,
        Match(AstNode.Labeled, [
            Match(AstNode.Label),
            Match(AstNode.Pointer, [
                Match(AstNode.Name)
            ])
        ])
    ).length == 0);
}

@("parser:var_expr1") unittest {
    mixin(parser_init("parser:var_expr2", "a : b()", "var_expr"));
    assert(diff(tree, Match(AstNode.VarExpression, [
        Match(AstNode.Name),
        Match(AstNode.Call, [
            Match(AstNode.Name),
            Match(AstNode.List)
        ]),
        Match(AstNode.None),
    ])).length == 0);
}

@("parser:var_expr2") unittest {
    mixin(parser_init("parser:var_expr3", "a:b=c", "var_expr"));
    assert(diff(tree, Match(AstNode.VarExpression, [
        Match(AstNode.Name),
        Match(AstNode.Name),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:name") unittest {
    mixin(parser_init("parser:name", "bks_1029", "expression"));
    assert(tree.type == AstNode.Name);
}

@("parser:int") unittest {
    mixin(parser_init("parser:int", "10294", "expression"));
    assert(tree.type == AstNode.Integer);
    assert(tree.value == 10_294);
}

@("parser:char") unittest {
    mixin(parser_init("parser:char", "'\n'", "expression"));
    assert(tree.type == AstNode.Char);
}

@("parser:list") unittest {
    mixin(parser_init("parser:list", "[,a, a:T, b=[4), c:k()=j]", "expression", false));
    assert(diff(tree, Match(AstNode.List, [
        Match(AstNode.ListMember, [
            Match(AstNode.None),
            Match(AstNode.None),
            Match(AstNode.Name),
        ]),
        Match(AstNode.ListMember, [
            Match(AstNode.Name),
            Match(AstNode.Name),
            Match(AstNode.None),
        ]),
        Match(AstNode.ListMember, [
            Match(AstNode.Name),
            Match(AstNode.None),
            Match(AstNode.Invalid)
        ]),
        Match(AstNode.ListMember, [
            Match(AstNode.Name),
            Match(AstNode.Call, [
                Match(AstNode.Name),
                Match(AstNode.List)
            ]),
            Match(AstNode.Name),
        ])
    ])).length == 0);
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingClosingDelimiter));
}

@("parser:list2") unittest {
    mixin(parser_init("parser:list2", "(a, b c)", "expression", false));
    assert(diff(tree, Match(AstNode.List, [
        Match(AstNode.ListMember, [
            Match(AstNode.None),
            Match(AstNode.None),
            Match(AstNode.Name),
        ]),
        Match(AstNode.Invalid),
    ])).length == 0);
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingSeparator));
}

@("parser:list3") unittest {
    mixin(parser_init("parser:list3", "(a b, c)", "expression", false));
    assert(diff(tree, Match(AstNode.List, [
        Match(AstNode.Invalid),
        Match(AstNode.ListMember, [
            Match(AstNode.None),
            Match(AstNode.None),
            Match(AstNode.Name),
        ])
    ])).length == 0);
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingSeparator));
    assert(parser.reporter.errors.length == 1);
}

@("parser:list4") unittest {
    mixin(parser_init("parser:list4", "[a b", "expression", false));
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingSeparator));
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingClosingDelimiter));
    assert(parser.reporter.errors.length == 2);
}

@("parser:list5") unittest {
    mixin(parser_init("parser:list4", "[a, [b]", "expression", false));
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingClosingDelimiter));
    assert(parser.reporter.errors.length == 1);
}

@("parser:list6") unittest {
    mixin(parser_init("parser:list4", "[[a) some * [other[]] _ stuff,]", "expression", false));
    assert(parser.reporter.has_error(SyntaxError.SequenceMissingClosingDelimiter));
    assert(parser.reporter.errors.length == 1);
}

@("parser:block") unittest {
    mixin(parser_init("parser:block", "{a\n2}", "expression"));
    assert(diff(tree, Match(AstNode.Block, [
        Match(AstNode.Name),
        Match(AstNode.Integer)
    ])).length == 0);
}

@("parser:function") unittest {
    mixin(parser_init("parser:func", "nah => blah * bleh", "expression"));
    assert(diff(tree, Match(AstNode.Function, [
        Match(AstNode.Name),
        Match(AstNode.Multiply, [
            Match(AstNode.Name),
            Match(AstNode.Name),
        ])
    ])).length == 0);
}

@("parser:call") unittest {
    mixin(parser_init("parser:call", "[][()]", "expression"));
    assert(diff(tree, Match(AstNode.Call, [
        Match(AstNode.List),
        Match(AstNode.List, [
            Match(AstNode.ListMember, [
                Match(AstNode.None),
                Match(AstNode.None),
                Match(AstNode.List),
            ])
        ])
    ])).length == 0);
}

@("parser:dot") unittest {
    mixin(parser_init("parser:dot", "a().c", "expression"));
    assert(diff(tree, Match(AstNode.Call, [
        Match(AstNode.Call, [
            Match(AstNode.Name),
            Match(AstNode.List)
        ]),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:path") unittest {
    import arc.stringtable: StringTable;

    mixin(parser_init("parser:path", "a::b::c::d", "expression"));
    assert(diff(tree, Match(AstNode.Path, [
        Match(AstNode.Path, [
            Match(AstNode.Path, [
                Match(AstNode.Name),
                Match(AstNode.Name),
            ]),
            Match(AstNode.Name),
        ]),
        Match(AstNode.Name)
    ])).length == 0);

    assert(tree.children[0].children[0].children[0].key == StringTable.digest("a"));
    assert(tree.children[0].children[0].children[1].key == StringTable.digest("b"));
    assert(tree.children[0].children[1].key == StringTable.digest("c"));
    assert(tree.children[1].key == StringTable.digest("d"));
}

@("parser:negate") unittest {
    mixin(parser_init("parser:negate", "-3", "expression"));
    assert(diff(tree, Match(AstNode.Negate, [Match(AstNode.Integer)])).length == 0);
}

@("parser:binary") unittest {
    //a = (b - c) + (d * ((e ^ f )^ g)) / h
    mixin(parser_init("parser:binary", "a = b - c + d * e ^ f ^ g / h", "expression"));
    assert(diff(tree, Match(AstNode.Assign, [
        Match(AstNode.Name),
        Match(AstNode.Add, [
            Match(AstNode.Subtract, [
                Match(AstNode.Name),
                Match(AstNode.Name),
            ]),
            Match(AstNode.Divide, [
                Match(AstNode.Multiply, [
                    Match(AstNode.Name),
                    Match(AstNode.Power, [
                        Match(AstNode.Name),
                        Match(AstNode.Power, [
                            Match(AstNode.Name),
                            Match(AstNode.Name),
                        ])
                    ])
                ]),
                Match(AstNode.Name)
            ])
        ])
    ])).length == 0);
}
