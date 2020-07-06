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
import arc.memory : VirtualMemory, gib;

struct SourceUnit {
    import std.algorithm: move;
    import arc.data.symbol : SymbolTable;

    enum source_memory_size = 4.gib;

public:
    string path;
    string text;
    VirtualMemory memory;
    AstNode*[] syntax_tree;
    SymbolTable* symbol_table;

    this(string path, string text, ref VirtualMemory vm, AstNode*[] syntax_tree, SymbolTable* symbols) {
        this.path = path;
        this.text = text;
        this.memory = move(vm);
        this.syntax_tree = syntax_tree;
        this.symbol_table = symbol_table;
    }

    @disable this(this);

    ~this() {
        destroy(memory);
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
    import arc.analysis.lexer: Token, TokenBuffer;
    import arc.analysis.parser : Parser, TreeAllocator;
    import arc.data.ast : Declaration, match;
    import arc.data.hash : Hash;
    import arc.data.stringtable : StringTable;
    import arc.data.structures : HashTable, KeyMap;
    import arc.data.symbol : Symbol;
    import arc.memory : ArrayPool;
    import arc.reporter : ArcError, Reporter;

    enum shared_vm_size = 128.gib;

public:
    this(Reporter* reporter) {
        _reporter = reporter;
        _shared_memory = VirtualMemory(shared_vm_size);

        _arrays = ArrayPool!(void*)(&_shared_memory);

        _stringtable = StringTable(64);
        _parser = Parser(_reporter, _token_buffer[], &_stringtable);
        _file_map = HashTable!(string, SourceUnit*)(64);
        _declarations = KeyMap!(AstNode*)(64);
    }

    ArcError[] update_file(string path, string text) {
        auto vm = VirtualMemory(SourceUnit.source_memory_size);
        auto allocator = TreeAllocator(&vm, cast(ArrayPool!(AstNode*)*) &_arrays);

        auto result = _parser.parse(text, allocator);
        if (auto ast = result.syntax_tree) {

            foreach (stmt; ast)
                stmt.match!void(
                        (Declaration* n) { _declarations.insert(n.symbol.name, n.header); },
                );
        }

        auto unit = _shared_memory.alloc!SourceUnit();
        *unit = SourceUnit(path, text, vm, result.syntax_tree, result.symbol_table);
        _file_map.insert(path, unit);
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
    Token[4096] _token_buffer;
    ArrayPool!(void*) _arrays;
    KeyMap!(AstNode*) _declarations;

    StringTable _stringtable;
}
