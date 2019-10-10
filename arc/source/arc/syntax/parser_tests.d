module arc.syntax.parser_tests;

version(unittest):

import arc.stringtable;
import arc.syntax.ast;
import arc.syntax.parser;
import arc.syntax.location;
import arc.syntax.syntax_reporter;

SyntaxReporter.ReportingFunction_token report_bad_token_set_flag = (r, t) {
    *(cast(bool*) r.user_data) = true;
};

string parser_init(string s) {
    import std.format: format;
    return "
        StringTable table;
        auto error = SyntaxReporter();
        auto text = SpannedText(0, %s, \"%s\");
        auto parser = Parser(text, &table, &error);
    ".format(s.length, s);
}

enum parser_done = "assert(parser.empty == true); assert(expr.span == text.span);";

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

@("parser:name") unittest {
    mixin(parser_init("bks_1029"));
    const expr = parser.primary();
    mixin(parser_done);
    assert(expr.type == AstNode.Name);
}

@("parser:int") unittest {
    mixin(parser_init("10294"));
    const expr = parser.primary();
    mixin(parser_done);
    assert(expr.type == AstNode.Integer);
    assert(expr.value == 10_294);
}

@("parser:char") unittest {
    mixin(parser_init("'\n'"));
    const expr = parser.primary();
    mixin(parser_done);
    assert(expr.type == AstNode.Char);
}

@("parser:list") unittest {
    mixin(parser_init("[3:T, b=[4), c:k()=j]"));
    bool not_closed;
    parser.error.user_data = &not_closed;
    parser.error.seq_not_closed_impl = (reporter, expr_loc, err_loc, type) {
        *(cast(bool*) reporter.user_data) = true;
    };

    auto expr = parser.primary();
    mixin(parser_done);
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
    assert(not_closed);
}

@("parser:function") unittest {
    mixin(parser_init("() => blah * bleh"));
    auto expr = parser.primary();
    mixin(parser_done);
    assert(diff(expr, Match(AstNode.Function, [
        Match(AstNode.List),
        Match(AstNode.Multiply, [
            Match(AstNode.Name),
            Match(AstNode.Name),
        ])
    ])).length == 0);
}

@("parser:call") unittest {
    mixin(parser_init("[][()]"));
    auto expr = parser.primary();
    mixin(parser_done);
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
    mixin(parser_init("a().c"));
    auto expr = parser.primary();
    mixin(parser_done);
    assert(diff(expr, Match(AstNode.Call, [
        Match(AstNode.Call, [
            Match(AstNode.Name),
            Match(AstNode.List)
        ]),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:negate") unittest {
    mixin(parser_init("-3"));
    auto expr = parser.primary();
    mixin(parser_done);
    assert(diff(expr, Match(AstNode.Negate, [Match(AstNode.Integer)])).length == 0);
}

@("parser:self_call") unittest {
    mixin(parser_init(".b.c"));
    auto expr = parser.primary();
    mixin(parser_done);
    assert(diff(expr, Match(AstNode.Call, [
        Match(AstNode.SelfCall, [
            Match(AstNode.Name)
        ]),
        Match(AstNode.Name)
    ])).length == 0);
}

@("parser:binary") unittest {
    //a = (b - c) + (d * ((e ^ f )^ g)) / h
    mixin(parser_init("a = b - c + d * e ^ f ^ g / h"));
    auto expr = parser.expression();
    mixin(parser_done);
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

@("parser:def") unittest {
    mixin(parser_init("def a: T = init"));
    auto expr = parser.def();
    mixin(parser_done);
    assert(diff(expr, Match(AstNode.Define, [
        Match(AstNode.Name),
        Match(AstNode.Name),
        Match(AstNode.Name),
    ])).length == 0);
}

@("parser:labeled_loop") unittest {
    mixin(parser_init("'label loop {}"));
    auto expr = parser.expression();
    mixin(parser_done);
    assert(diff(expr, Match(AstNode.Labeled, [
        Match(AstNode.Label),
        Match(AstNode.Loop, [
            Match(AstNode.Block)
        ])
    ])).length == 0);
}

@("parser: if_else") unittest {
    mixin(parser_init("if a {} else if b {} else { break }"));
    auto expr = parser.expression();
    mixin(parser_done);
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
