/**
 This module contains the code for creating scope trees, which are used to
 determine when and where symbol_nodes come in to and out of scope.
 */
module arc.semantic.scope_tree;

import arc.data.ast : AstNode;
import arc.data.hash : Key;
import arc.data.symbol;
import arc.memory;

struct ScopeTreeNode {
    enum Kind {
        Scope,
        FileScope,
        Symbol,
        UnresolvedSymbol,
    }

    Kind kind;
    ScopeTreeNode* parent;

    union {
        Appender!(ScopeTreeNode*) children;
        Symbol* symbol;
        AstNode* unresolved_node;
    }

    this(Kind kind, ScopeTreeNode* parent) {
        this.kind = kind;
        this.parent = parent;
    }

    this(ArrayPool!(ScopeTreeNode*)* arrays)
    in(arrays) {
        this(Kind.FileScope, null);
        this.children = Appender!(ScopeTreeNode*)(arrays);
    }

    this(ScopeTreeNode* parent, ArrayPool!(ScopeTreeNode*)* arrays)
    in(parent && arrays) {
        this(Kind.Scope, parent);
        this.children = Appender!(ScopeTreeNode*)(arrays);
    }

    this(ScopeTreeNode* parent, Symbol* symbol)
    in(parent && symbol) {
        this(Kind.Symbol, parent);
        this.symbol = symbol;
    }

    this(ScopeTreeNode* parent, AstNode* unresolved_node)
    in(parent && unresolved_node) {
        this(Kind.UnresolvedSymbol, parent);
        this.unresolved_node = unresolved_node;
    }
}

struct ScopeBuilder {
    ScopeTreeNode* root, current;

    ObjectPool!Symbol* symbols;
    TreeAllocator!ScopeTreeNode* scopes;

    this(ObjectPool!Symbol* symbol_mem, TreeAllocator!ScopeTreeNode* scope_mem)
    in(symbol_mem && scope_mem) {
        symbols = symbol_mem;
        scopes = scope_mem;

        root = current = scope_mem.objects.alloc(&scopes.arrays);
    }

    void push_scope()
    in (root &&  current && symbols && scopes) {
        auto new_scope = scopes.objects.alloc(current, &scopes.arrays);
        current.children ~= new_scope;
        current = new_scope;
    }

    Symbol* add_symbol(Symbol.Kind kind, Key text)
    in (root &&  current && symbols && scopes) {
        auto symbol = symbols.alloc(kind, text);
        current.children ~= scopes.objects.alloc(current, symbol);
        return symbol;
    }

    void add_unresolved_symbol(AstNode* node)
    in(node && root &&  current && symbols && scopes) {
        node.is_resolved_symbol = false;
        node.symbol = Symbol.unresolved;
        current.children ~= scopes.objects.alloc(current, node);
    }

    void pop_scope()
    in(current !is root && root &&  current && symbols && scopes) {
        current = current.parent;
    }
}
