module arc.data.hash;

alias Hash = ulong;

/**
 * Hashes text and returns (key: ubyte[8], text: const(char)[])
 */
Hash digest(const(char)[] text) pure {
    import std.digest.murmurhash : hash = digest, MurmurHash3;

    auto key = hash!(MurmurHash3!(128))(text);

    return (cast(ulong*) key.ptr)[0];
}

Hash hash_of(T)(auto ref T t) {
    import std.digest.murmurhash : hash = digest, MurmurHash3;

    static if (__traits(compiles, t.toHash()))
        return t.toHash();
    
    auto key = hash!(MurmurHash3!128)((&t)[0 .. 1]);
    return (cast(ulong*) key.ptr)[0];
}
