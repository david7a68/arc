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

@("Virtual Array") unittest {
    struct T {
        int a, b;
    }

    auto va = VirtualArray!T(1.kib);

    foreach(i; 0 .. 20)
        va ~= T(i, 20 - i);
    
    assert(va[1] is va.base_ptr[1]);

    foreach (i; 0 .. 20) {
        assert(va[i] == T(i, 20 - i));
        assert(va[i] is va.base_ptr[i]);
    }
}
