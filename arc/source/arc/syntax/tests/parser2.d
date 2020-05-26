module arc.syntax.tests.parser2;

import arc.data.ast2;
import arc.syntax.parser2;
import arc.reporter;

Reporter reporter;
ParsingContext parser;
AstNodeAllocator nodes = void;

static this() {
    nodes = new AstNodeAllocator();
    parser = ParsingContext(&reporter, &nodes);
}

struct ParseResult {
    AstNode* tree;
    string text;
}

auto parse(string op)(string text) {
    parser.begin(text);
    reporter.clear();
    mixin ("return ParseResult(parse_" ~ op ~ "(&parser), text);");
}

bool type_equivalent(ParseResult result, AstNode.Kind[] types...) {
    import std.algorithm: equal;
    import std.stdio: writefln;

    AstNode.Kind[] tree_types;

    void flatten(AstNode* n) {
        if (n is null) return;
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
    assert(check_types("a_name".parse!"expression", AstNode.Kind.Name));
}

@("Parse Int") unittest {
    assert(check_types("20".parse!"expression", AstNode.Kind.Integer));
}

@("Parse Char") unittest {
    assert(check_types("'a'".parse!"expression", AstNode.Kind.Char));
}

@("Parse Unary") unittest {
    with (AstNode.Kind) {
        assert(check_types("-var".parse!"expression", Negate, Name));
    }
}

@("Parse Binary") unittest {
    with (AstNode.Kind) {
        assert(check_types("1 + 1".parse!"expression", Add, Integer, Integer));
        assert(check_types("a ^ 2".parse!"expression", Power, Name, Integer));

        assert(check_types("-a * b".parse!"expression",
            Multiply,
                Negate,
                    Name,
                Name
        ));

        assert(check_types("a - -3".parse!"expression",
            Subtract,
                Name,
                Negate,
                    Integer
        ));

        assert(check_types("b - c + d * e ^ f ^ g / h".parse!"expression",
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
    }
}
