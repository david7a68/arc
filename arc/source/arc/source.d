module arc.source;

alias StringID = ulong;

struct Source {
    StringID id;
    const(char)[] name;
    const(char)[] buffer;
}

/**
 * TextSet is a non-removable set which stores immutable references to long
 * strings. Not thread-safe.
 */
struct Sources {
    struct ReservedSlot {
        StringID id;
        const(char)[] name;
        char[] buffer;
    }

    /**
     * Reserves a slot of `source_size` bytes called `name`.
     *
     * ```
     * StringID load_file(Sources files, string name) {
     *     auto file = fs.get_file_info(name);
     *     const slot = files.reserve(file.size);
     *     fs.read(name, slot.buffer);
     *     return slot.id;
     * }
     * ```
     *
     * Returns: A slot with a mutable buffer
     */
    ReservedSlot reserve(string name, size_t source_size) {
        const id = name.hashOf;

        const name_start = name_buffer.length;
        name_buffer ~= name;

        const text_start = text_buffer.length;
        text_buffer.length = text_start + source_size;

        auto source = Source(id, name_buffer[name_start .. $], text_buffer[text_start .. $]);
        sources[id] = source;

        return ReservedSlot(id, source.name, text_buffer[text_start .. $]);
    }

    /**
     * Inserts a string called `name` into the set.
     *
     * Returns: A read-only slot representing the string
     */
    Source insert(string s, string name) {
        auto source = reserve(name, s.length);
        source.buffer[] = s[];
        return Source(source.id, source.name, cast(const) source.buffer);
    }

    /**
     * Retrieves a read-only slot corresponding to the string ID.
     *
     * Note: This function will return a null-slot if `id` does not exist.
     */
    Source opIndex(StringID id) {
        return sources.get(id, Source());
    }

private:
    Source[ulong] sources;
    char[] name_buffer;
    char[] text_buffer;
}
