module arc.data.hash;

alias Key = uint;

/**
 * Hashes text and returns (key: ubyte[8], text: const(char)[])
 */
Key digest(const(char)[] text) pure {
    import std.digest.murmurhash: hash = digest, MurmurHash3;

    ubyte[4] key = hash!(MurmurHash3!(32))(text);

    return (cast(uint*) key.ptr)[0];
}
