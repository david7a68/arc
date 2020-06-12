module arc.util;

string case_of(T)(const T[] c...) {
    import std.algorithm : map;
    import std.array : join;
    import std.conv: to;

    return c.map!(v => "case " ~ v.to!int.to!string ~ ": ").join().to!string;
}
