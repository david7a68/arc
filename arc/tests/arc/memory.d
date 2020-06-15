module tests.arc.memory;

import arc.memory;

@("Virtual Memory") unittest {
    auto vm = VirtualMemory(1.kib);

    const m1 = vm.alloc(10);
    assert(m1.ptr !is null);
    assert(m1.length == 10);
}

@("Object Pool") unittest {
    auto vm = VirtualMemory(1.kib);

    struct T {
        bool a;
        size_t b;
    }

    auto ts = ObjectPool!T(&vm);

    auto t1 = ts.alloc();
    assert(t1 !is null);
    // assert(ts._chunks._first_free_node is null);

    auto t2 = ts.alloc();
    assert(t2 !is null);
    // assert(ts._chunks._first_free_node is null);

    ts.free(t1);
    // assert(ts._chunks._first_free_node is cast(void*) t1);

    auto t3 = ts.alloc();
    assert(t3 !is null);
    // assert(ts._chunks._first_free_node is null);
    assert(t3 == t1);

    ts.free(t2);
    ts.free(t3);

    assert(ts.alloc() == t3);
    assert(ts.alloc() == t2);
}

@("Object Pool 2") unittest {
    class T {
        bool a;
        size_t b;
    }

    auto vm = VirtualMemory(1.kib);
    auto ts = ObjectPool!T(&vm);

    auto t1 = ts.alloc();
    assert(t1);
    t1.b = 3;

    auto t2 = ts.alloc();
    assert(t2 && t2.b == 0);

    ts.free(t1);
    ts.free(t2);

    assert(ts.alloc() == t2);
    assert(ts.alloc() == t1);
}

@("Array Allocator") unittest {
    auto memory = VirtualMemory(1024);
    auto allocator = ArrayPool!uint(&memory, [3, 5, 8]);

    // assert(allocator._chunks.length == 3);
    // assert(allocator._chunks[0].chunk_size == allocator.Header.sizeof + 3 * uint.sizeof);
    // assert(allocator._chunks[1].chunk_size == allocator.Header.sizeof + 5 * uint.sizeof);
    // assert(allocator._chunks[2].chunk_size == allocator.Header.sizeof + 8 * uint.sizeof);

    auto v = 100u;

    auto c = allocator.alloc_size_class(0);
    assert(c.length == 3);
    c[1] = v;
    allocator.expand(c);
    assert(c.length == 5);
    assert(c[1] is v);
    allocator.expand(c);
    assert(c.length == 8);
    assert(c[1] is v);
    allocator.free(c);
    const d = allocator.alloc_size_class(2);
    assert(c.ptr == d.ptr);

    auto appender = allocator.get_appender();
    foreach (i; 0 .. 6)
        appender ~= i;

    assert(appender.get().length == 6);
    // assert(appender._array.length == 8);

    foreach (i; 0 .. 6)
        // assert(appender._array[i] == i);
        assert(appender.get()[i] == i);
}

@("Array Allocator with Reference Types") unittest {
    class T {
        bool a;
        size_t b;
    }

    auto vm = VirtualMemory(1.kib);
    auto ts = ObjectPool!T(&vm);
    auto as = ArrayPool!T(&vm);

    auto t1 = ts.alloc();
    auto t2 = ts.alloc();
    auto a1 = as.alloc_size_class(0);
    // assert(as.header_of(a1).class_index == 0);
    assert(a1.length == 2);

    a1[0] = t1;
    assert(a1[0] is t1);
    assert(a1[1] is null);

    a1[1] = t2;
    assert(a1 == [t1, t2]);

    as.expand(a1);
    // assert(as.header_of(a1).class_index == 1);
    assert(a1.length == 4); 
    assert(a1 == [t1, t2, null, null]);

    as.free(a1);
    assert(as.alloc_size_class(1) is a1);
}

@("Nesting Array Allocators") unittest {
    auto vm = VirtualMemory(1.kib);
    auto as = ArrayPool!uint(&vm);

    auto a1 = as.get_appender();
    a1 ~= 1;
    a1 ~= 2;
    a1 ~= 3;

    auto a2 = as.get_appender();
    a2 ~= 4;
    a2 ~= 5;

    a1 ~= 6;

    assert(a1.get() == [1, 2, 3, 6]);
    assert(a2.get() == [4, 5]);
}
