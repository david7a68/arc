module arc.syntax.parser_tests;

version(unittest):

import arc.stringtable;
import arc.syntax.ast;
import arc.syntax.parser;
import arc.syntax.location;
import arc.syntax.reporter;

string parser_init(string s, string parser_name, bool err_check = true) {
    import std.format: format;
    return "StringTable table;
        auto error = new SyntaxReporter();
        auto text = SpannedText(0, %s, \"%s\");
        auto parser = Parser(text, &table, error);
        auto expr = parser.%s();
        assert(parser.empty == true);
        %s".format(s.length, s, parser_name, err_check ? "assert(parser.reporter.errors.length == 0);" : "");
}


struct Match {
    AstNode.Type node_type;
    Match[] children;
}

AstNode[] diff(AstNode root, Match match) {
    AstNode[] r;
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
    mixin(parser_init("def a: T = init;", "statement"));
    assert(diff(expr, Match(AstNode.Define, [
        Match(AstNode.Name),
        Match(AstNode.Name),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:break") unittest {
    {
        mixin(parser_init("break;", "statement"));
        assert(diff(expr, Match(AstNode.Break, [
            Match(AstNode.None),
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("break 'here;", "statement"));
        assert(diff(expr, Match(AstNode.Break, [
            Match(AstNode.Label),
            Match(AstNode.None),
        ])).length == 0);
    }
    {
        mixin(parser_init("break -a * b;", "statement"));
        assert(diff(expr, Match(AstNode.Break, [
            Match(AstNode.None),
            Match(AstNode.Multiply, [
                Match(AstNode.Negate, [ Match(AstNode.Name) ]),
                Match(AstNode.Name)
            ]),
        ])).length == 0);
    }
    {
        mixin(parser_init("break 'here a;", "statement"));
        assert(diff(expr, Match(AstNode.Break, [
            Match(AstNode.Label),
            Match(AstNode.Name)
        ])).length == 0);
    }
    {
        mixin(parser_init("{break}", "statement"));
        assert(diff(expr, Match(AstNode.Block, [
            Match(AstNode.Break, [
                Match(AstNode.None),
                Match(AstNode.None),
            ])
        ])).length == 0);
    }
}

@("parser:return") unittest {
    {
        mixin(parser_init("{return}", "statement"));
        assert(diff(expr, Match(AstNode.Block, [
            Match(AstNode.Return, [
                Match(AstNode.None),
                Match(AstNode.None),
            ])
        ])).length == 0);
    }
}

@("parser:continue") unittest {
    {
        mixin(parser_init("{continue}", "statement"));
        assert(diff(expr, Match(AstNode.Block, [
            Match(AstNode.Continue, [
                Match(AstNode.None),
            ])
        ])).length == 0);
    }
}

@("parser:name") unittest {
    mixin(parser_init("bks_1029", "expression"));
    assert(expr.type == AstNode.Name);
}

@("parser:int") unittest {
    mixin(parser_init("10294", "expression"));
    assert(expr.type == AstNode.Integer);
    assert(expr.value == 10_294);
}

@("parser:char") unittest {
    mixin(parser_init("'\n'", "expression"));
    assert(expr.type == AstNode.Char);
}

@("parser:list") unittest {
    mixin(parser_init("[3:T, b=[4), c:k()=j]", "expression", false));
    assert(diff(expr, Match(AstNode.List, [
        Match(AstNode.VarExpression, [
            Match(AstNode.Integer),
            Match(AstNode.Name),
            Match(AstNode.None),
        ]),
        Match(AstNode.VarExpression, [
            Match(AstNode.Name),
            Match(AstNode.None),
            Match(AstNode.Invalid)
        ]),
        Match(AstNode.VarExpression, [
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

@("parser:function") unittest {
    mixin(parser_init("() => blah * bleh", "expression"));
    assert(diff(expr, Match(AstNode.Function, [
        Match(AstNode.List),
        Match(AstNode.Multiply, [
            Match(AstNode.Name),
            Match(AstNode.Name),
        ])
    ])).length == 0);
}

@("parser:call") unittest {
    mixin(parser_init("[][()]", "expression"));
    assert(diff(expr, Match(AstNode.Call, [
        Match(AstNode.List),
        Match(AstNode.List, [
            Match(AstNode.VarExpression, [
                Match(AstNode.None),
                Match(AstNode.None),
                Match(AstNode.List),
            ])
        ])
    ])).length == 0);
}

@("parser:dot") unittest {
    mixin(parser_init("a().c", "expression"));
    assert(diff(expr, Match(AstNode.Call, [
        Match(AstNode.Call, [
            Match(AstNode.Name),
            Match(AstNode.List)
        ]),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:negate") unittest {
    mixin(parser_init("-3", "expression"));
    assert(diff(expr, Match(AstNode.Negate, [Match(AstNode.Integer)])).length == 0);
}

@("parser:self_call") unittest {
    mixin(parser_init(".b.c", "expression"));
    assert(diff(expr, Match(AstNode.Call, [
        Match(AstNode.SelfCall, [
            Match(AstNode.Name)
        ]),
        Match(AstNode.Name)
    ])).length == 0);
}

@("parser:binary") unittest {
    //a = (b - c) + (d * ((e ^ f )^ g)) / h
    mixin(parser_init("a = b - c + d * e ^ f ^ g / h", "expression"));
    assert(diff(expr, Match(AstNode.Assign, [
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

@("parser:loop") unittest {
    mixin(parser_init("loop {}", "expression"));
    assert(diff(expr, Match(AstNode.Loop, [
            Match(AstNode.Block)
    ])).length == 0);
}

@("parser:labeled_loop") unittest {
    mixin(parser_init("'label loop {}", "expression"));
    assert(diff(expr, Match(AstNode.Labeled, [
        Match(AstNode.Label),
        Match(AstNode.Loop, [
            Match(AstNode.Block)
        ])
    ])).length == 0);
}

@("parser:if_else") unittest {
    mixin(parser_init("if a {} else if b {} else { break }", "expression"));
    assert(diff(expr, Match(AstNode.If, [
        Match(AstNode.Name),
        Match(AstNode.Block),
        Match(AstNode.If, [
            Match(AstNode.Name),
            Match(AstNode.Block),
            Match(AstNode.Block, [
                Match(AstNode.Break, [
                    Match(AstNode.None),
                    Match(AstNode.None)
                ])
            ])
        ])
    ])).length == 0);
}
