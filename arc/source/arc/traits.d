module arc.traits;

import std.traits: FunctionAttribute, SetFunctionAttributes, functionAttributes, functionLinkage;

auto assume_pure(T)(T t) {
    enum attrs = functionAttributes!T | FunctionAttribute.pure_;
    return cast(SetFunctionAttributes!(T, functionLinkage!T, attrs)) t;
}
