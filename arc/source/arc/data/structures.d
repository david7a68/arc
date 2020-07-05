module arc.data.structures;

import arc.data.hash: Hash = Hash, hash_of;
import arc.memory: VirtualMemory, ObjectPool, gib;
import std.traits: isCopyable;

/**
 Generic hash table implementation making use of backing virtual memory. It does
 not manage any memory beyond that required for the key/value pairing, so any
 memory pointed to by the key or value will not be cleaned up when elements are
 removed from the hash table.
 */
struct HashTable(Key, Value) {
    static assert(isCopyable!Value, "Values must be of a copyable type.");

public:
    this(size_t initial_size) {
        _map = KeyMap!Entry(initial_size);
    }

    bool contains(Key key) {
        return _map.contains(key.hash_of);
    }

    Value get(Key key)
    in (contains(key)) {
        foreach (ref entry; (*_map.bucket_for(hash_of(key)))[])
            if (entry.hash == hash_of(key))
                return entry.value.value;
        assert(false);
    }

    Value get(Key key, lazy Value key_not_found) {
        // dip into the implementation of KeyMap to preserve ref-ness
        foreach (ref entry; (*_map.bucket_for(key.hash_of))[])
            if (entry.hash == key.hash_of)
                return entry.value.value;
        return key_not_found;
    }

    bool insert(Key key, Value value) {
        return _map.insert(key.hash_of, Entry(key, value));
    }

    Value take(Key key) {
        return _map.take(key.hash_of).value;
    }

private:
    struct Entry {
        Key key;
        Value value;
    }

    KeyMap!Entry _map;
}

struct KeyMap(Value) {
    enum maximum_size = 64.gib;
    enum max_chain_length = 16;

    static assert(isCopyable!Value, "Values must be of a copyable type.");

public:
    this(size_t initial_size) {
        _bucket_vm = VirtualMemory(maximum_size);
        _node_vm = VirtualMemory(maximum_size);
        _entries = ObjectPool!(Bucket.Node)(&_node_vm);
        _buckets = (cast(Bucket*) _bucket_vm.alloc(initial_size * Bucket.sizeof).ptr)[0 .. initial_size];

        foreach (ref bucket; _buckets)
            bucket = Bucket(&_entries);
    }

    bool contains(Hash hash) {
        foreach (ref entry; (*bucket_for(hash))[])
            if (entry.hash == hash)
                return true;
        return false;
    }

    Value get(Hash hash)
    in (contains(hash)) {
        foreach (ref entry; (*bucket_for(hash))[])
            if (entry.hash == hash)
                return entry.value;
        assert(false);
    }

    Value get(Hash hash, lazy Value key_not_found) {
        foreach (ref entry; (*bucket_for(hash))[])
            if (entry.hash == hash)
                return entry.value;
        return key_not_found;
    }

    bool insert(Hash hash, Value element) {
        auto bucket = bucket_for(hash);
        if (bucket.length == max_chain_length) {
            resize();
            bucket = bucket_for(hash);
        }

        // Make insert update entries if they already exist
        foreach (ref entry; (*bucket)[]) {
            if (entry.hash == hash) {
                entry.value = element;
                return true;
            }
        }

        bucket.append(Entry(hash, element, _generation));
        return false;
    }

    Value take(Hash hash) {
        static predicate(size_t i, ref Entry e, Hash hash) {
            return e.hash == hash;
        }
        return bucket_for(hash).take!predicate(hash).value;
    }

private:
    alias Bucket = Slist!(Entry, true);

    struct Entry {
        Hash hash;
        Value value;
        ubyte generation;
        ubyte[7] padding;
    }

    ubyte _generation;
    Bucket[] _buckets;
    VirtualMemory _bucket_vm, _node_vm;
    ObjectPool!(Bucket.Node) _entries;

    Bucket* bucket_for(Hash hash) { return &_buckets[hash.value % _buckets.length]; }

    void resize() {
        const old_length = _buckets.length;
        _bucket_vm.alloc(old_length * Bucket.sizeof);
        _buckets = _buckets.ptr[0 .. _buckets.length * 2];

        foreach (ref bucket; _buckets[old_length .. $])
            bucket = Bucket(&_entries);

        _generation++;
        foreach (i, ref bucket; _buckets[0 .. old_length]) {
            while (bucket.length && bucket.get(0).generation != _generation) {
                auto entry = bucket.take(0);
                entry.generation = _generation;
                bucket_for(entry.hash).append(entry);
            }
        }
    }
}

/**
 * A singly-linked list implementation backed by a virtual object pool. This
 * object pool may be shared between lists of the same entity type, or owned
 * by the list.
 */
struct Slist(ElementType, bool shared_pool = false) {
    import std.algorithm: move;

    struct Node {
        Node* next;
        ElementType value;
    }

public:
    static if (shared_pool)
        this(ObjectPool!Node* nodes) { _nodes = nodes; }
    else
        this(VirtualMemory* vm) { _nodes = ObjectPool!Node(vm); }

    // dfmt off
    uint length() { return _length; }

    auto opIndex() {
        struct Range {
            Node* current;
            bool empty() { return current is null; }
            void popFront() { current = current.next; }
            ref ElementType front() in (current) {
                return current.value;
            }
        }

        return Range(_first);
    }
    // dfmt on

    /// Appends an element to the end of the list.
    void append(ElementType e) {
        auto node = _nodes.alloc(null);
        node.value = move(e);
        (_first ? _last.next : _first) = node;
        _last = node;
        _length++;
    }

    /// Retrieves a reference to the element at the i'th index.
    ref ElementType get(size_t i) in (i < length) {
        auto n = _first;
        for (; i > 0; n = n.next, i--) {}
        return n.value;
    }

    static default_remove_predicate(size_t index, ref ElementType value, size_t target_index) {
        return index == target_index;
    }

    /// Retrieves the first element that satisfies predicate. Additional
    /// arguments to the predicate may be provided in the argument list. The
    /// default predicate cause `remove` to remove the element at index `t`.
    ElementType take(alias predicate = default_remove_predicate, Args...)(Args args) {
        Node*[2] pe = [null, _first];
        Node* node = pe[1];
        for (size_t i = 0; node && !predicate(i, node.value, args); node = node.next, i++) {
            pe[0] = pe[1];
            pe[1] = node;
        }

        if (!node)
            assert(false);

        (pe[0] ? pe[0].next : _first) = pe[1].next;
        _length--;

        scope (exit) _nodes.free(pe[1]);
        return pe[1].value;
    }

private:
    Node* _first, _last;

    static if (shared_pool)
        ObjectPool!Node* _nodes;
    else
        ObjectPool!Node _nodes;

    uint _length;
}
