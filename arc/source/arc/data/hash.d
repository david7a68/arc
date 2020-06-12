module arc.data.hash;

alias Key = ulong;

/**
 * Hashes text and returns (key: ubyte[8], text: const(char)[])
 */
Key digest(const(char)[] text) pure {
    import std.digest.murmurhash : hash = digest, MurmurHash3;

    auto key = hash!(MurmurHash3!(128))(text);

    return (cast(ulong*) key.ptr)[0];
}
