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
        _symbol_table = GlobalSymbolTable(initial_symbol_table_size);
        _ast_allocator = new AstAllocator();
        _scope_allocator = ScopeAllocator(&_symbol_table);

        _parser = new Parser(
            _reporter,
            &_string_table,
            _ast_allocator,
            &_scope_allocator,
            &_symbol_table
        );
    }

    StringTable* string_table() return { return &_string_table; }

    const(char)[] string_of(Hash hash) {
        return _string_table[hash];
    }

    SyntaxTree ast_of(Source* source) {
        return _syntax_source_map.require(source, _parser.parse(source));
    }

    AstNode* ast_of(AstNodeId id) {
        return _ast_allocator.ast_of(id);
    }

    Span span_of(AstNodeId id) {
        return _ast_allocator.span_of(id);
    }

    ReturnType match(ReturnType, Ops...)(AstNodeId id, Ops ops) if (Ops.length > 0) {
        return _ast_allocator.match!ReturnType(id, ops);
    }

    AstNodeId[] children_of(AstNodeId id) {
        return _ast_allocator.children_of(id);
    }

    Symbol* symbol_of(AstNodeId id) {
        // dfmt off
        return _ast_allocator.match!(Symbol*)(id,
            (ListMember* lm) => _symbol_table[lm.symbol],
            (Definition* df) => _symbol_table[df.symbol],
            (Variable* vr) => _symbol_table[vr.symbol],
            (SymbolRef* sr) {
                // Implement symbol searching here. We depend on all source
                // files being present by the time this is called. Otherwise
                // there is no way to reliably resolve imported symbols.
                return null;
            },
            (AstNodeId* n) => null);
        // dfmt on
    }

    const(char)[] name_of(AstNodeId id) {
        auto symbol = symbol_of(id);
        return symbol ? _string_table[symbol.name] : [];
    }

    /// Retrieves the declaration site of a `SymbolRef`.
    AstNode* declaration_of(AstNodeId id) {
        assert(false, "Not Implemented");
    }

    TypeId type_of(AstNodeId id) {
        assert(false, "Not Implemented");
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
