module arc.syntax.ast;

class AstNodeVisitor {
    import std.traits: EnumMembers;

    static foreach (member; EnumMembers!(AstNode.Type)) {
        import std.format: format;
        import std.conv: to;
        import std.uni: toLower;

        mixin("void visit_%s(AstNode* n) { visit_children(n); }".format(member.to!string.toLower));
    }

    protected void visit_children(AstNode* n) in (n.first_child !is null) {
        auto next = n.first_child;
        
        do {
            auto current = next;
            next = next.next;
            current.accept(this);
        } while (next && next !is n.first_child);
    }
}

struct AstNode {
    enum Type {
        Invalid,
        Tuple, Name, Integer
    }

    alias Type this;

    Type type;
    const(char)* start;
    size_t span;
    
    AstNode* prev, next;
    AstNode* first_child;

    void add_child(AstNode* child) {
        if (first_child is null)
            first_child = child;
        else {
            child.prev = first_child.prev;
            child.prev.next = child;

            child.next = first_child;
            next.prev = child;
        }
    }

    void accept(AstNodeVisitor v) {
        switch (type) with (Type) {
        case Invalid:
            v.visit_invalid(&this); break;
        case Tuple:
            v.visit_tuple(&this); break;
        case Name:
            v.visit_name(&this); break;
        case Integer:
            v.visit_integer(&this); break;
        default: assert(false);
        }
    }
}


alias NodeMemory = NodeMemoryImpl!(10 * 4096);

/**
 * Linked-block node allocator.
 */
struct NodeMemoryImpl(size_t items_per_block) {
    static assert(items_per_block > 0, "Why would you do such a thing?");

    /// A block of memory within
    struct Block {
        /// The next block in the pool.
        /// This pointer is mostly here so that we can clean up properly (not that we do yet)
        Block* next;
        /// The memory stored by the pool.
        AstNode[items_per_block] memory;
    }

    /// The first element of a singly-linked list of memory blocks
    Block* blocks;
    /// The number of elements in use in the current block including elements on the free list
    size_t last_block_occupancy;
    /// The dummy root node of the free list
    AstNode freelist_root;

    /// Initialized the memory pool. Make sure to initialize with this function.
    static NodeMemoryImpl setup() {
        NodeMemoryImpl mem;
        mem.clear();
        return mem;
    }

    /// Allocates a node from the pool
    AstNode* alloc() {
        AstNode* result;
        if (freelist_root.next !is &freelist_root) {
            // Take the first element of the free list. Who knows, it may still be in cache.
            result = freelist_root.next;
            freelist_root.next = result.next;
            freelist_root.prev = result.prev;
        }
        else if (last_block_occupancy < blocks.memory.length) {
            result = &blocks.memory[last_block_occupancy];
            last_block_occupancy++;
        } else {
            auto block = alloc_block();
			assert(block);
            block.next = blocks;
            blocks = block;

            last_block_occupancy = 1;
            result = &block.memory[0];
        }

        *result = AstNode();
        result.next = result;
        result.prev = result;
        return result;
    }

    /// Returns a node and all of its children to the pool
    void free(AstNode* node) in ((node !is null) && (node !is &freelist_root)) {
        if (node.first_child)
            free_children(node);
        
        free_only(node);
    }

    /// Return a node to the pool
    void free_only(AstNode* node) in ((node !is null) && (node !is &freelist_root)) {
        // reset the node
        *node = AstNode();
        
        node.next = freelist_root.next;
        freelist_root.next.prev = node;
        node.prev = &freelist_root;
        freelist_root.next = node;
    }

    /// Returns a node's children to the pool and clears the node.first_child pointer
    void free_children(AstNode* node) in ((node !is null) && (node !is &freelist_root)) {
        auto next = node.first_child;
        
        do {
            auto current = next;
            next = next.next;
            free(current);
        } while (next && next !is node.first_child);

        node.first_child = null;
    }

    /// Resets the pool.
    void clear() {
        // @Cleanup: currenlty relies on the GC for deleting buffers
        // @Todo: Fix this
        blocks = alloc_block;
        freelist_root.next = &freelist_root;
        freelist_root.prev = &freelist_root;
    }

private:
    Block* alloc_block() {
        // @Cleanup: handle out of memory conditions
        return new Block;
    }
}

unittest {
    import std.algorithm: uniq;
    import std.array: array;

    auto mem = NodeMemoryImpl!4.setup();
    AstNode*[20] n;

    foreach (i; 0 .. n.length)
        n[i] = mem.alloc();

    // check that every element is unique (no freelist wierdness)
    assert(n[].uniq.array.length == n.length);
    
    foreach (i; 0 .. n.length)
        mem.free(n[i]);
    
    // check that every element was correctly added to the free-list
    foreach_reverse (i; 0 .. n.length)
        assert(n[i] == mem.alloc());
    
    const old_block = mem.blocks;
    mem.clear();
    const new_block = mem.blocks;
    assert(old_block !is new_block);
}

unittest {
	auto mem = NodeMemoryImpl!4.setup();
	AstNode*[20] n;
	
	foreach (i; 0 .. n.length)
	   n[i] = mem.alloc();
	
	foreach (i; 0 .. 9)
	   n[0].add_child(n[1 + i]);
	
	foreach (i; 0 .. 10)
	   n[0].first_child.add_child(n[10 + i]);
	
	foreach (i; 0 .. n.length)
	   assert(*n[i] != AstNode());
	
	mem.free_children(n[0]);
	foreach (i; 1 .. n.length) {
        assert(n[i].type == AstNode.Invalid);
        assert(n[i].start is null);
        assert(n[i].span == 0);
        assert(n[i].first_child is null);
    }
	assert(*n[0] != AstNode());
	
	mem.free(n[0]);
}
