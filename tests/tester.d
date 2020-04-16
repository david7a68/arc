/+ dub.sdl:
name "tester"
+/
module tester;

struct TesterOptions {
    string executable;
}

TesterOptions get_options(string[] args) {
    import std.getopt: getopt;

    TesterOptions options;

    options.executable = args[1];

    return options;
}

string[] load_test(string path) {
    import std.array: array;
    import std.algorithm: splitter, startsWith;
    import std.file: readText;
    import std.ascii: isWhite;

    auto source = readText(path);
    auto lines = source.splitter("\n");

    string[] result;

    if (lines.front.startsWith("#test")) {
        auto name_split = lines.front.splitter!isWhite;
        name_split.popFront();
        result ~= name_split.front;
        lines.popFront();

        while (!lines.front.startsWith("#"))
            lines.popFront();
        
        if (!lines.empty && lines.front.startsWith("#source")) {
            lines.popFront();
            
            string src;

            while (!lines.empty && lines.front.startsWith("    ")) {
                src ~= lines.front;
                lines.popFront();
            }

            result ~= src;
        }

        if (!lines.empty && lines.front.startsWith("#expected")) {
            lines.popFront();

            string expected;

            while (!lines.empty && lines.front.startsWith("    ")) {
                expected ~= lines.front;
                lines.popFront();
            }

            result ~= expected;
        }
    }

    return result;
}

struct SyntaxTest {
    string name;
    string source;
    string expected;

    bool passed;
}

void run_syntax_tests() {
    void load_syntax_test(string path) {
        auto strings = load_test(path);

        import std.stdio;

        writeln(strings);

        import std.regex: ctRegex, replaceAll;

        auto source = strings[1].replaceAll(ctRegex!`\s\s+`, " ");
        writeln(source);
    }
    // load all tests in tests/syntax
    // check first line for name
    // check next line for expected
    // if following line is comment, append to expected
    // run test 

    load_syntax_test("hello.arc");
}

void main(string[] args) {
    const options = get_options(args);

    run_syntax_tests();
}
