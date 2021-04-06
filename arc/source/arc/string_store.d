module arc.string_store;

import shard.memory : gib, Allocator, VirtualAllocator, page_size;
import shard.math_util : round_to_next;
import shard.hash : Hash64, hash64_of;
import shard.pad : pad_bytes;
import std.algorithm : filter;
import core.stdc.string : memset;

/**
Fixed-location store for interning strings.
*/
struct StringStore {
    enum initial_entry_size = 64;
    enum max_string_store_size = 4.gib;

    this(Allocator struct_allocator, VirtualAllocator* string_allocator, uint max_probes = 32) {
        _struct_allocator = struct_allocator;
        _vm = string_allocator;
        _max_probes = max_probes;

        _entries = _struct_allocator.make_array!Entry(initial_entry_size);
        _string_array = cast(char[]) _vm.reserve(max_string_store_size);
    }

    @disable this(this);

    ~this() {
        if (_vm && _struct_allocator) {
            _vm.free(_string_array);
            _struct_allocator.dispose(_entries);
        }
    }

    size_t size() {
        return _num_saved;
    }

    Hash64 save(const char[] str) {
        const hash = hash64_of(str);
        ulong index = hash.value % _entries.length, probes = 0;
        while (true) {
            auto slot = &_entries[index];

            if (slot.hash == Hash64()) {
                if (_chars_written + str.length > max_string_store_size)
                    assert(0, "String store at capacity!");

                if (_chars_committed < _chars_written + str.length) {
                    const needed = (_chars_written + str.length - _chars_committed);
                    const aligned = round_to_next(needed, page_size);

                    _vm.commit(_string_array[_chars_committed .. _chars_committed + aligned]);
                    _chars_committed += aligned;
                }

                slot.hash = hash;
                slot.first = _chars_written;
                slot.one_past_last = _chars_written + cast(uint) str.length;

                _string_array[_chars_written .. _chars_written + str.length] = str;
                _chars_written += str.length;

                _num_saved++;
                return hash;
            }
            else if (slot.hash == hash) {
                // String is already present in the map.
                return slot.hash;
            }
            else {
                index = (index + 1) % _entries.length;
                probes++;

                if (probes == _max_probes) {
                    _rehash();
                    probes = 0;
                    index = hash.value % _entries.length;
                    continue;
                }

                // Prefer rare branch (at most once per get()) than modulo every
                // loop iteration.
                if (index == _entries.length)
                    index = 0;
            }
        }

        assert(0, "Unreachable");
    }

    const(char[]) get(Hash64 id) {
        ulong index = id.value % _entries.length, probes = 0;
        while (probes < _max_probes && _entries[index].hash != id) {
            index = (index + 1) % _entries.length;
            probes++;

            // Prefer rare branch (at most once per get()) than modulo every
            // loop iteration.
            if (index == _entries.length)
                index = 0;
        }

        auto entry = &_entries[index];
        return entry.hash == id ? _string_array[entry.first .. entry.one_past_last] : null;
    }

private:
    struct Entry {
        Hash64 hash;
        uint first;
        uint one_past_last;

        static assert(Entry.sizeof == 16);
    }

    void _rehash() {
        Entry[] new_entries = _struct_allocator.make_array!Entry(_entries.length * 2);

        rehash: while (true) {
            map_entries: for (auto i = 0; i < _entries.length; i++) {
                auto entry = &_entries[i];

                if (_entries[i].hash == Hash64())
                    continue map_entries;

                ulong index = entry.hash.value % new_entries.length, probes = 0;

                // Find empty slot.
                while (probes < _max_probes && new_entries[index].hash != Hash64()) {
                    index = (index + 1) % new_entries.length;
                    probes++;

                    // Prefer rare branch (at most once per get()) than modulo
                    // every loop iteration.
                    if (index == new_entries.length)
                        index = 0;
                }

                // Grow if probe length exceeded.
                if (probes == _max_probes) {
                    _struct_allocator.resize_array(new_entries, new_entries.length * 2);
                    memset(new_entries.ptr, 0, new_entries.sizeof * new_entries.length);
                    continue rehash;
                }

                assert(new_entries[index].hash == Hash64());
                new_entries[index] = *entry;

            }
            break rehash;
        }

        _struct_allocator.dispose(_entries);
        _entries = new_entries;
    }

    Allocator _struct_allocator;
    VirtualAllocator* _vm;

    uint _num_saved;
    uint _max_probes;

    Entry[] _entries;

    uint _chars_written;
    uint _chars_committed;
    char[] _string_array;
}

unittest {
    import shard.memory : AllocatorApi, SysAllocator, Arena, MemoryTracker, MemoryStats;
    import std.format : format;

    auto struct_mem = new AllocatorApi!(MemoryTracker!SysAllocator)();
    VirtualAllocator vm;
    auto store = StringStore(struct_mem, &vm);
    
    void test_mem_stats(size_t struct_allocs, size_t string_allocs) {
        MemoryStats stats;
        struct_mem.impl.get_stats(stats);
        assert(stats.num_allocations == struct_allocs);
    }

    const a = store.save("0");
    assert(store.size == 1);
    assert(store.get(a) == "0");
    test_mem_stats(1, 1);
    assert(store.save("0") == a);
    test_mem_stats(1, 1);

    // Make sure that we cross page boundaries.
    foreach (i; 1 .. 8192) {
        store.save(format!"%s"(i));
        assert(store.size == i + 1);
        test_mem_stats(1, i + 1);
    }

    destroy(store);
    test_mem_stats(0, 0);
}
