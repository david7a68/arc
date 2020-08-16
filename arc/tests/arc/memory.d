module tests.arc.memory;

import arc.memory;

@("Units of Memory") unittest {
    assert(1.kib == 1024);
    assert(100.kib == 100 * 1.kib);

    assert(1.mib == 1024 * 1024);
    assert(1.mib == 1024.kib);
    assert(100.mib == 100 * 1.mib);

    assert(1.gib == 1024 * 1024 * 1024);
    assert(1.gib == 1024.mib);
    assert(100.gib == 100 * 1.gib);
}

@("Virtual Memory") unittest {
    auto vm = VirtualMemory(1.kib);

    const m1 = vm.alloc(10);
    assert(m1.ptr !is null);
    assert(m1.length == 10);

    assert(vm.bytes_reserved == 4.kib);
    assert(vm.bytes_allocated == 16);

    const m2 = vm.alloc(4.kib - 16);
    assert(vm.bytes_reserved == vm.bytes_allocated);
}

@("Object Pool") unittest {
    struct T {
        static int dtored;

        bool a;
        size_t b;

        ~this() {
            dtored++;
        }
    }

    auto vm = VirtualMemory(1.kib);
    auto ts = ObjectPool!T(&vm);
    assert(vm.alloc_size_for(T.sizeof) == ts.chunk_size);
    assert(vm.bytes_allocated == 0);
    assert(ts.bytes_reserved == 0);
    assert(ts.objects_reserved == 0);

    auto t1 = ts.alloc();
    assert(t1);
    assert(ts.objects_allocated == 1 && ts.objects_reserved == 1);
    assert(ts.bytes_reserved == ts.chunk_size);

    auto t2 = ts.alloc();
    assert(t2 && t2.b == 0);
    assert(ts.objects_allocated == 2 && ts.objects_reserved == 2);
    assert(ts.bytes_reserved == 2 * ts.chunk_size);

    ts.free(t1);
    ts.free(t2);
    assert(T.dtored == 2);
    assert(ts.objects_allocated == 0 && ts.objects_reserved == 2);
    assert(ts.bytes_allocated == 0 && ts.bytes_reserved == 2 * ts.chunk_size);

    assert(ts.alloc() == t2);
    assert(ts.alloc() == t1);
}

@("Object Pool 2") unittest {
    class T {
        static int dtored;

        bool a;
        size_t b;

        ~this() {
            dtored++;
        }
    }

    auto vm = VirtualMemory(1.kib);
    auto ts = ObjectPool!T(&vm);
    assert(vm.alloc_size_for(__traits(classInstanceSize, T)) == ts.chunk_size);
    assert(vm.bytes_allocated == 0);
    assert(ts.bytes_reserved == 0);
    assert(ts.objects_reserved == 0);

    auto t1 = ts.alloc();
    assert(t1);
    assert(ts.objects_allocated == 1 && ts.objects_reserved == 1);
    assert(ts.bytes_reserved == ts.chunk_size);

    auto t2 = ts.alloc();
    assert(t2 && t2.b == 0);
    assert(ts.objects_allocated == 2 && ts.objects_reserved == 2);
    assert(ts.bytes_reserved == 2 * ts.chunk_size);

    ts.free(t1);
    ts.free(t2);
    assert(T.dtored == 2);
    assert(ts.objects_allocated == 0 && ts.objects_reserved == 2);
    assert(ts.bytes_allocated == 0 && ts.bytes_reserved == 2 * ts.chunk_size);

    assert(ts.alloc() == t2);
    assert(ts.alloc() == t1);
}

@("Array Allocator") unittest {
    auto memory = VirtualMemory(1024);
    auto allocator = ArrayPool!uint(&memory, [3, 5, 8]);

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

    foreach (i; 0 .. 6)
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
    assert(a1.length == 2);

    a1[0] = t1;
    assert(a1[0] is t1);
    assert(a1[1] is null);

    a1[1] = t2;
    assert(a1 == [t1, t2]);

    as.expand(a1);
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
