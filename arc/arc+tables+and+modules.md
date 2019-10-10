All types in Arc have access to a meta-table, which provides information about the type.

For example, a list's meta-table contains the type, name, and order of every element, a self referential reference to the meta-table, as well as the names of any functions which are bound to it.

These functions are effectively given a type-specific alias by which they may be referred to globally. This means that the file that defined the type need not be known, so long as the name mangling matches. (The mangling is passed on when a file is imported, which is bound with the mangled object when the compiled artifacts are linked together).

Unnamed types do get a meta-table, but the table is type specific, rather than name-type specific.

The meta-table construct may be extended to include module definition and 'import' statements. An 'import' statement simply makes all non-non-automatic table members accessible without needing to refer to the table first. This means that modules in Arc must be defined in a way similar to C++, where the absence of a namespace means that everything gets dumped into the global namespace.

An enum may be constructed thusly:

1. Define a type derived from the type that all members of the enumeration will share
2. For each member of the enum, place its name and value within the enumeration's meta-table
3. Make sure to cast each member's value to the derived type to ensure some measure of type safe usage

Alternatively, it will be possible to construct an enum by calling the function enum on a list of named elements of the same value once macros arw implemented. The function will create a dummy type, set all of the meta-table members, then return it to be bound by the user's define statement.