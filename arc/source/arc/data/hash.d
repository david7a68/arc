module arc.data.hash;

struct Hash {
    ubyte[8] value;

    ulong toHash() const {
        return *(cast(ulong*) &value[0]);
    }

    bool opEquals(Hash other) const {
        return value == other.value;
    }
}

/**
 * Hashes text and returns (key: ubyte[8], text: const(char)[])
 */
Hash hash_of(T: const(char)[])(T text) pure {
    import std.digest.murmurhash : hash = digest, MurmurHash3;

    auto key = hash!(MurmurHash3!(128))(text);

    return Hash(key[0 .. 8]);
}

Hash hash_of(T)(auto ref T t) {
    import std.digest.murmurhash : hash = digest, MurmurHash3;

    static if (__traits(compiles, t.toHash())) {
        auto key = t.toHash();
        return Hash((cast(ubyte*) &key)[0 .. 8]);
    }
    else {
        auto key = hash!(MurmurHash3!128)((&t)[0 .. 1]);
        return Hash(key[0 .. 8]);
    }
}
