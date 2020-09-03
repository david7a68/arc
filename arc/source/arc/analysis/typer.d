module arc.analysis.typer;

import arc.data.ast;
import arc.data.symbol;
import arc.data.type;
import arc.data.stringtable;
import arc.data.scopes;

final class Typer {

    enum null_type = TypeId(0);

public:
    this(ArcTypeAllocator types, AstAllocator nodes, StringTable* strings, GlobalSymbolTable symbols, ScopeAllocator scopes) {
        _types = types;
        _nodes = nodes;
        _scopes = scopes;
        _symbols = symbols;

        initialize_builtin_types(strings, symbols);
    }

    SymbolId[] builtin_type_symbols() {
        return [_int_type_symbol, _char_type_symbol, _const_int_type_symbol];
    }

    TypeId type_id_of(AstNodeId node) {
        return _nodes.match!TypeId(node,
            (Integer* it) => _symbols[_const_int_type_symbol].type_id,
            (Char* ch) => _symbols[_char_type_symbol].type_id,
            (ListMember* lm) {
                if (lm.type_id != TypeId())
                    return lm.type_id;

                if (_nodes.ast_of(lm.type_expr).kind != AstNode.Kind.InferredType)
                    lm.type_id = type_id_of(lm.type_expr);
                else
                    lm.type_id = type_id_of(lm.init_expr);

                return lm.type_id;
            },
            (Definition* df) {
                if (df.type_id != TypeId())
                    return df.type_id;

                if (_nodes.ast_of(df.type_expr).kind != AstNode.Kind.InferredType)
                    df.type_id = type_id_of(df.type_expr);
                else
                    df.type_id = type_id_of(df.init_expr);

                return df.type_id;
            },
            (Variable* vr) {
                if (vr.type_id != TypeId())
                    return vr.type_id;

                if (_nodes.ast_of(vr.type_expr).kind != AstNode.Kind.InferredType)
                    vr.type_id = type_id_of(vr.type_expr);
                else
                    vr.type_id = type_id_of(vr.init_expr);

                return vr.type_id;
            },
            (SymbolRef* sr) => type_id_of(sr),
            (AstNode* n) => TypeId());
    }

    TypeId type_id_of(SymbolRef* sr) {
        if (sr.header.type_id != null_type)
            return sr.header.type_id;

        Symbol* decl;
        auto scope_ = _scopes.scope_of(sr.outer_scope_id);
        while (scope_ && decl is null) {
            decl = scope_.get_local(sr.name_hash);
            scope_ = scope_.parent;
        }

        if (decl is null)
            assert(false, "TODO: Symbol not found.");

        if (decl.type_id == null_type) {
            type_id_of(decl.declaration_id);
        }

        sr.type_id = decl.type_id;
        return decl.type_id;
    }

    TypeId type_id_of(List* ls) {
        assert(false, "Not Implemented.");
    }

private:
    void initialize_builtin_types(StringTable* strings, GlobalSymbolTable symbols) {
        SymbolId make_type(ArcType.Kind kind, string name, size_t size, size_t alignment) {
            const symbol = symbols.make_symbol(Symbol.Kind.TypeName, strings.intern(name));
            const type = _types.save_type(ArcType(kind, symbol, size, alignment));
            symbols[symbol].type_id = type;

            return symbol;
        }

        // Reserve TypeId(0) for error conditions.
        make_type(ArcType.Kind.Unknown, "", 0, 0);

        // Ideally, these would all be defined in a different file.
        _int_type_symbol = make_type(ArcType.Kind.Integer, "int", 8, 8);
        _char_type_symbol = make_type(ArcType.Kind.Char, "char", 1, 1);
        _const_int_type_symbol = make_type(ArcType.Kind.ConstInteger, "__arc_const_int", 8, 8);
    }

    AstAllocator _nodes;
    ScopeAllocator _scopes;
    GlobalSymbolTable _symbols;

    ArcTypeAllocator _types;

    SymbolId _int_type_symbol, _char_type_symbol, _const_int_type_symbol;
}