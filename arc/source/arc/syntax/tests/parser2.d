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

auto parse(string op)(string text) {
    parser.begin(text);
    mixin ("return parse_" ~ op ~ "(&parser);");
}

bool type_equivalent(AstNode* tree, AstNode.Kind[] types...) {
    import std.algorithm: equal;

    AstNode.Kind[] tree_types;

    void flatten(AstNode* n) {
        if (n is null) return;
        tree_types ~= n.kind;

        foreach (child; n.children)
            flatten(child);
    }

    flatten(tree);
    import std.stdio; writeln(tree_types);
    return equal(types, tree_types);
}

bool check_types(AstNode* node, AstNode.Kind[] types...) {
    return type_equivalent(node, types);
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
        assert(check_types("-var".parse!"expression", Negate, Name));
    }
}
