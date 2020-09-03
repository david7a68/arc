module arc.analysis.source_analyzer;

import arc.analysis.lexer;
import arc.analysis.parser : IParser, Parser;
import arc.data.ast;
import arc.data.hash;
import arc.data.scopes;
import arc.data.span;
import arc.data.stringtable;
import arc.data.symbol;
import arc.reporter : Reporter;
import arc.source : Source;

struct SourceAnalyzer {

    enum initial_string_table_size = 128;
    enum initial_symbol_table_size = 64;
    enum initial_syntax_map_size = 64;

public:
    this(Reporter* reporter) {
        _reporter = reporter;

        // _string_table does not have a constructor
        _symbol_table = new GlobalSymbolTable(initial_symbol_table_size);

        _ast_allocator = new AstAllocator();
        _scope_allocator = new ScopeAllocator(_symbol_table);

        _parser = new Parser(
            _reporter,
            &_string_table,
            _ast_allocator,
            _scope_allocator,
            _symbol_table
        );
    }

    /**
     Initial entry point into syntax analysis.

     Adding a source file will cause its syntax tree to be generated, and any
     imported files to be added recursively.
     */
    void add_source(Source* source) {
        _syntax_source_map.require(source, _parser.parse(source));
    }

    /**
     Retrieves the syntax tree of a file that has been previously processed.

     It is an error to call this function with a source that has not been passed
     to `add_source(Source*)`.
     */
    SyntaxTree ast_of(Source* source) {
        return _syntax_source_map[source];
    }

    AstNode* ast_of(AstNodeId id) {
        return _ast_allocator.ast_of(id);
    }

    ReturnType match(ReturnType, Ops...)(AstNodeId id, Ops ops) if (Ops.length > 0) {
        return _ast_allocator.match!ReturnType(id, ops);
    }

    AstNodeId[] children_of(AstNodeId id) {
        return _ast_allocator.children_of(id);
    }

    const(char)[] name_of(SymbolId id) {
        return _string_table[_symbol_table[id].name];
    }

    SymbolId symbol_id_of(AstNodeId id) {
        return _ast_allocator.match!SymbolId(id,
            (ListMember* lm) => lm.symbol,
            (Definition* df) => df.symbol,
            (Variable* vr) => vr.symbol,
            (SymbolRef* sr) {
                if (sr.declaration != SymbolId())
                    return sr.declaration;

                auto scope_ = _scope_allocator.scope_of(sr.outer_scope_id);
                
                SymbolId symbol;
                for (Scope* s = scope_; s && symbol == SymbolId(); s = s.parent)
                    symbol = s.get_local_id(sr.name_hash);
                
                return symbol;
            },
            (AstNode* n) => SymbolId());
    }

    StringTable* string_table() return { return &_string_table; }

    const(char)[] string_of(Hash hash) {
        return _string_table[hash];
    }

private:
    Reporter* _reporter;

    StringTable _string_table;
    GlobalSymbolTable _symbol_table;

    AstAllocator _ast_allocator;
    ScopeAllocator _scope_allocator;

    IParser _parser;

    /// Uses GC because I'm lazy right now.
    SyntaxTree[Source*] _syntax_source_map;
}
