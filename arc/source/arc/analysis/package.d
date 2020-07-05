/**
 This module (`arc.analysis`) contains types and functions for drawing semantic
 information from source code. This includes the parser, lexer, symbol
 resolver, and type checker.

 The primary interface to the analysis module is the `SourceAnalyzer` struct,
 where files are submitted and processed, and the results are queried.

 Prescription:
    If a package of source files passes is processed by the analyzer without
    error, the package should be semantically free of errors.
 */
module arc.analysis;

import arc.data.ast : AstNode;
import arc.memory : VirtualMemory;

struct SourceUnit {
    import arc.data.symbol : SymbolTable;

public:
    string path;
    string text;
    VirtualMemory memory;
    AstNode*[] syntax_tree;
    SymbolTable symbol_table;

    this(string path, string text, size_t memory_size) {
        this.path = path;
        this.text = text;
        this.memory = VirtualMemory(memory_size);
        this.symbol_table = SymbolTable(&memory);
    }

    auto coords_of(uint position) {
        struct Coordinates {
            uint line;
            uint column;
        }

        auto coords = Coordinates(1, 0);
        foreach (c; text[0 .. position])
            if (c == '\n') {
                coords.column = 1;
                coords.line++;
            }
            else
                coords.column++;

        return coords;
    }
}

/**
 The abstract syntax tree is tightly integrated to symbolic and type information,
 so it makes sense to keep code that operates on these structures close together.
 Additionally, it is common for an operation for resolving a symbol to require
 type information, or for type deduction to require symbolic information, such
 that clear-cut passes are not helpful.

 Once a file is 'uploaded' to the analyzer, queries can be run against the
 analyzed source, such as for a symbol, its type, and the syntax tree that
 represents its initialization. These queries are cached so they only ever run
 the first time they are called. This caching behavior poses some challenges if
 the analyzer is to be run in such a way that files can be swapped and updated,
 but that's not our concern at this time.
 */
struct SourceAnalyzer {
    import arc.analysis.parser : Parser, ParseUnit;
    import arc.data.ast : Declaration, match;
    import arc.data.hash : Hash;
    import arc.data.stringtable : StringTable;
    import arc.data.structures : HashTable, KeyMap;
    import arc.data.symbol : Symbol;
    import arc.memory : ArrayPool, gib;
    import arc.reporter : ArcError, Reporter;

    enum shared_vm_size = 128.gib;
    enum file_vm_size = 4.gib;

public:
    this(Reporter* reporter) {
        _reporter = reporter;
        _shared_memory = VirtualMemory(shared_vm_size);

        _parser = Parser();
        _arrays = ArrayPool!(void*)(&_shared_memory);

        _file_map = HashTable!(string, SourceUnit*)(64);
        _stringtable = StringTable(64);
        _declarations = KeyMap!(AstNode*)(64);
    }

    ArcError[] update_file(string path, string text) {
        auto unit = _shared_memory.alloc!SourceUnit(path, text, file_vm_size);
        _file_map.insert(path, unit);

        // auto pu = ParseUnit(unit, cast(ArrayPool!(AstNode*)*)*&_arrays, _reporter, &_stringtable);

        auto pu = ParseUnit(
                &unit.memory,
                cast(ArrayPool!(AstNode*)*)&_arrays,
                _reporter,
                &unit.symbol_table,
                &_stringtable,
                unit.text);

        if (auto syntax = _parser.parse(pu)) {
            unit.syntax_tree = syntax;

            foreach (stmt; syntax)
                stmt.match!void(
                        (Declaration* n) { _declarations.insert(n.symbol.name, n.header); },
                );

            return [];
        }

        return _reporter.errors;
    }

    StringTable* stringtable() return  {
        return &_stringtable;
    }

    SourceUnit* get_file(string path) {
        return _file_map.get(path, cast(SourceUnit*) null);
    }

    AstNode* ast_for(Hash hash) {
        return _declarations.get(hash, null);
    }

    Symbol* symbol_for(Hash hash) {
        if (auto decl = ast_for(hash))
            return (cast(Declaration*) decl).symbol;
        return null;
    }

private:
    Reporter* _reporter;
    VirtualMemory _shared_memory;
    HashTable!(string, SourceUnit*) _file_map;

    Parser _parser;
    ArrayPool!(void*) _arrays;
    KeyMap!(AstNode*) _declarations;

    StringTable _stringtable;
}
