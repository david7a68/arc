module tests.arc.structures;

import arc.data.hash : Hash, hash_of;
import arc.data.structures;
import arc.memory;
import std.random : uniform;

@("HashTable") unittest {
    import std.algorithm: move;
    
    struct T {
        long value;
    }

    struct Pair {
        ulong key;
        T value;
    }

    Pair[20_000] pairs;
    foreach (i; 0 .. pairs.length)
        pairs[i] = Pair(uniform(0, ulong.max), T(i));
    
    auto ht = HashTable!(ulong, T)(64);
    foreach (ref p; pairs)
        ht.insert(p.key, move(p.value));
    
    foreach (ref p; pairs)
        ht.contains(p.key);
    
    foreach (ref p; pairs)
        assert(ht.get(p.key) == p.value);
}

@("KeyMap") unittest {
    struct Pair {
        Hash key;
        long value;
    }

    Pair[10_000] pairs;
    foreach (i; 0 .. 10_000)
        pairs[i] = Pair(hash_of(uniform(0, ulong.max)), i);

    auto km = KeyMap!long(15);
    foreach (p; pairs)
        km.insert(p.key, p.value);

    foreach (p; pairs)
        km.contains(p.key);

    long v = -1;
    foreach (p; pairs)
        assert(km.get(p.key, v) == p.value);
}

@("Slist") unittest {
    auto vm = VirtualMemory(16.kib);
    auto sl = Slist!uint(&vm);

    foreach (i; 0 .. 1_000)
        sl.append(1_000 - i);

    foreach (i; 0 .. 1_000)
        assert(sl.get(i) == 1_000 - i);
    
    int count;
    foreach (i; sl[]) {
        assert(1_000 - i == count);
        count++;
    }
    assert(sl.length == 1_000);

    foreach (i; 0 .. 1_000)
        assert(sl.take(0) == 1_000 - i);
    assert(sl.length == 0);
}
