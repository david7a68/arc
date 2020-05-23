module arc.data.ast2;

import arc.data.source: Span;
import arc.data.hash: Key;

alias AstNodeIndex = uint;
enum InvalidAstNodeIndex = AstNodeIndex.max;

alias ArcTypeIndex = uint;
enum InvalidArcTypeIndex = ArcTypeIndex.max;

struct AstNode {
    enum Kind : ubyte {
        None,
        Invalid,
        Inferred,
        ConstantDeclaration,
        TypeDeclaration,
        Variable,
        Name,
        Integer,
        Char,
        List,
        Block,
        Unary,
        Binary,
        Call,
        Access,
        Function,
        FunctionType,
    }

    Span span;
    Kind kind;

    ubyte[4] padding;

    AstNodeIndex next = InvalidAstNodeIndex;
    ArcTypeIndex type = InvalidArcTypeIndex;

    union {
        AstNodeIndex first_child = InvalidAstNodeIndex;
        Key name;
    }
}

/**
 * A simple helper struct for constructing a list of AST nodes. It can be a bit
 * troublesome to assemble singly-linked lists in-order where the first element
 * is the head. This struct allows you to simply call add() repeatedly to make
 * this work. Once you are done with the list, simply copy out the `head` member
 * for use.
 */
struct AstNodeList {
    /// The region that all of these nodes are from.
    AstNodeAllocator* alloc;
    /// The index of the first element in this list.
    AstNodeIndex head;
    /// The current tail of the list.
    AstNode* current;

    this(AstNodeAllocator* nodes, AstNodeIndex first) {
        alloc = nodes;
        head = first;
        current = alloc.get(first);
    }

    /**
     * Adds a node to the end of the list.
     *
     * In debug mode, this will check that the current tail of the list does not
     * have its `next` member assigned to.
     */
    void add(AstNodeIndex next_index) {
        assert(current.next == InvalidAstNodeIndex, "Cannot add more than one tail to a list");

        current.next = next_index;
        current = alloc.get(next_index);
    }
}

/**
 * 
 */
struct AstNodeAllocator {
    import arc.memory: IndexedRegion;

    /// The maximum number of nodes that we can have active at any one time.
    /// This should be large enough that we never see it. 128 Gib of AST nodes,
    /// anyone?
    enum max_nodes = AstNodeIndex.max - 1;

    /// The region from which the AST nodes are allocated.
    IndexedRegion!(AstNode, AstNodeIndex) nodes;

    /// The index of the first free node. We could stash this in the next member
    /// of the reserved '0' node, but we leave it out for readability.
    AstNodeIndex first_free_node = InvalidAstNodeIndex;

    /// The number of nodes that are currently active.
    size_t num_allocated;

    @disable this();        // Disable constructor. We have a 0-arg init function.
    @disable this(this);    // Disable copying. Nodes holds pointers that must
                            // be unique or allocations may fail.

    static AstNodeAllocator initialize() {
        return AstNodeAllocator(max_nodes);
    }

    ~this() {
    }

    private this(size_t num_nodes) {
        nodes = IndexedRegion!(AstNode, AstNodeIndex)(num_nodes);
    }

    bool is_at_capacity() {
        return num_allocated >= nodes.num_allocated && nodes.is_at_capacity;
    }

    /**
     * Allocates a node from the pool.
     */
    AstNodeIndex alloc() in (!is_at_capacity) {
        // Invariant: When num_allocated < nodes.num_allocated, there are
        // nodes.num_allocated - num_allocated nodes in the freelist
        // We don't have to care about the value of the last freelist node's
        // .next becaseu we check for capacity first.

        scope (exit) num_allocated++;

        if (num_allocated < nodes.num_allocated) {
            auto index = first_free_node;
            first_free_node = nodes.get(first_free_node).next;
            return index;
        }

        auto index = nodes.alloc();
        return index;
    }

    /**
     * Frees the node and returns it to the pool. This function will also free
     * any children or, if this node is part of a list, any nodes following it.
     *
     * Do not call this function in a loop unless you know that there are no
     * repeating references to any node. Attempting to free a node multiple
     * times is an error, but is not currently caught programmatically.
     */
    void free(AstNodeIndex node_index) {
        for (AstNodeIndex current = node_index, next; current != InvalidAstNodeIndex; current = next) {
            auto node = nodes.get(current);
            next = node.next;

            // Free the node's children, adding them to the free list
            if (node.first_child != InvalidAstNodeIndex) {
                free(node.first_child);
                node.first_child = InvalidAstNodeIndex;
            }

            // Add self to the free list
            node.next = first_free_node;
            first_free_node = current;
            num_allocated--;
        }
    }

    ///
    AstNode* get(AstNodeIndex index) {
        return nodes.get(index);
    }
}

@("AST allocator")
unittest {
    auto nodes = AstNodeAllocator.initialize();

    auto list = AstNodeList(&nodes, nodes.alloc());
    for (int i = 0; i < 31; i++) // for 32 nodes total
        list.add(nodes.alloc());

    assert(nodes.num_allocated == 32);

	nodes.free(list.head);
    assert(nodes.num_allocated == 0);
}
