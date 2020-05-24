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

    /// The span of text used to derive this syntax node, including all of its
    /// children.
    Span span; // This is currently the largest part of the node, can we shrink it by putting them elsewhere?
    
    /// Discriminant, provides information for how to interpret this AST node
    Kind kind;

    /// Padding, may be used for internal purposes. DO NOT USE.
    ubyte[7] padding;   // liveness marking really only needs 1 bit, we could add a parent or prev pointer here.

    /// The index of the next sibling. These form a singly linked list.
    AstNodeIndex next = InvalidAstNodeIndex;

    /// The index of the data-type associated with this node. This is empty until
    /// until the typechecking phase.
    ArcTypeIndex type = InvalidArcTypeIndex;

    union {
        /// The first child of this node. Children are represented as singly
        /// linked lists.
        AstNodeIndex first_child = InvalidAstNodeIndex;

        /// The interned symbol name for this node.
        Key name;

        /// The key to the string value represented by this node.
        Key string_value;

        /// The integral value represented by this value.
        ulong int_value;
    }
}

static assert(AstNode.sizeof == 32);

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
 * A free-list allocator backed by virtual memory. This allocator will not
 * return memory to the operating system until the allocator itself is destroyed
 * much like a Region.
 *
 * The goals for this allocator are to provide a simple, fast allocator for
 * creating nodes that may be 'destroyed' at any time as parts of the tree are
 * found to be invalid during parsing or semantic analysis. Additionally, passes
 * over the tree may rewrite parts of it, requiring that nodes by quickly and
 * efficiently freed or allocated without wasting too many computer resources.
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
     *
     * This function makes use of the padding space in AstNodes. Do not modify
     * this space!
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

            // Fill padding to indicate that this node is 'live'
            nodes.get(index).padding[] = 255;
            return index;
        }

        auto index = nodes.alloc();
        // Fill padding to indicate that this node is 'live'
        nodes.get(index).padding[] = 255;
        return index;
    }

    /**
     * Frees the node and returns it to the pool. This function will also free
     * any children or, if this node is part of a list, any nodes following it.
     *
     * Do not call this function in a loop unless you know that there are no
     * repeating references to any node. Attempting to free a node multiple
     * times is an error, but is not currently caught programmatically.
     *
     * This function makes use of the padding space in AstNodes, and may
     * overwrite anything you put there.
     */
    void free(AstNodeIndex node_index) {
        for (AstNodeIndex current = node_index, next; current != InvalidAstNodeIndex; current = next) {
            auto node = nodes.get(current);

            // Check that the node has not been freed previously... unless it
            // has and then been reallocated.
            assert(node.padding == [255, 255, 255, 255, 255, 255, 255]);

            next = node.next;

            // Free the node's children, adding them to the free list
            if (node.first_child != InvalidAstNodeIndex) {
                free(node.first_child);
                node.first_child = InvalidAstNodeIndex;
            }

            // Add self to the free list
            node.next = first_free_node;
            first_free_node = current;
            
            // Freed nodes have empty padding
            node.padding[0 .. 2] = 0;
            
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
