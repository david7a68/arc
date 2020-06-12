/**
 This module contains the code for creating scope trees, which are used to
 determine when and where symbols come in to and out of scope.
 */
module arc.semantic.scope_tree;

import arc.data.ast : AstNode;
import arc.data.symbol : Symbol;
import arc.memory : TreeAllocator;

struct Scope {
    enum Kind {
        List,
        Block,
        Function,
        File
    }

    Kind kind;
    Scope* outer;
    Symbol*[] symbols;

    @disable this(this);
}

struct ScopeTreeBuilder {
    struct ScopeRef {
        Scope* scope_ptr;
        ScopeTreeBuilder* builder;
        alias scope_ptr this;

        this(ScopeTreeBuilder* builder, Scope* ptr) {
            this.builder = builder;
            scope_ptr = ptr;
        }

        ~this() {
            builder.pop_scope();
        }
    }

public:
    Scope* top_scope;
    Scope* current_scope;

    this(TreeAllocator!Scope* allocator) {
        _scope_allocator = allocator;
        top_scope = _scope_allocator.alloc(Scope.Kind.File);
    }

    ScopeRef push_scope(Scope.Kind kind) {
        current_scope = _scope_allocator.alloc(kind, current_scope);
        return ScopeRef(&this, current_scope);
    }

    void pop_scope() {
        current_scope = current_scope.outer;
    }

private:
    TreeAllocator!Scope* _scope_allocator;
}

Scope* gather_scopes(TreeAllocator!Scope* allocator, AstNode*[] syntax) {
    auto builder = ScopeTreeBuilder(allocator);
    foreach (node; syntax)
        gather_scopes(&builder, node);
    return builder.top_scope;
}

void gather_scopes(ScopeTreeBuilder* b, AstNode* syntax) {

}
