module arc.hash;

alias Key = ubyte[8];

/**
 * Hashes text and returns (key: ubyte[8], text: const(char)[])
 */
Key digest(const(char)[] text) {
    import std.digest.murmurhash: hash = digest, MurmurHash3;

    ubyte[16] key = hash!(MurmurHash3!(128))(text);

    return key[0 .. 8];
}
