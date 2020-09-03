# Arc

## Parts

### Namespaces

Arc treats namespaces as accessible objects which may be retrieved, stored, and manipulated.

The following operations are defined on a namespace:

* Importing
* Accessing (Querying)

### Declarations

Declarations in Arc are divided into static and variable.

Static declarations are things such as types, constants, and functions. Their values are determined at compile-time, and never change.

Variables are just that, variable, with their value determined at runtime.

Declarations have three parts, a name, a type, and a value.

The type and value parts are optional, so long as the other is present. That is, a declaration with a type may infer the value, and vice versa.

### Lists

Lists are finite, ordered sequence of elements. Logically, each element is a variable (a declaration), with all that entails.

A list does three things:

 1. Order and index its elements starting from 0
 2. Form a name space with which to contain the variables
 3. Represent a block of memory containing all of its members in sequence

This means that a list's members are addressable both by index and by name.

List members may optionally elide both name and type. When this occurs, the element becomes addressable only by index, and the type is inferred from the value.

### Types

There are two kinds of types, primitives, and lists.

Lists are constructed from protoypes, created using the `def` keyword. Whenever the name is invoked outside of a type expression, a copy of the list is made and placed on the stack. If the list defines a default funciton, that may be called immediately after. If the name is invoked within a type expression, it is treated as the type itself.

If a list does not have a default function, one is generated for it, allowing for the copying of data into the new instance of the list.

```arc
def ListNode : (next: ListNode*);

head : ListNode = ListNode(null);
```

### Functions

### Expressions

* Function Calls
* Math (+ - * / %)
* Logic (and or == < > <= >= != is !is ^)
* Constants

## Symbol Resolution Algorithm

* Lexical scopes
* Top-level declarations must either have a type specifier, or have their types inferrable by their initialization expression
