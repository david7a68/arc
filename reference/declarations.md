# Declarations

A declaration is an operation which binds a name (symbol) to additional data. There are three different kinds of declarations in Arc: definitions, variables, and list members.

## Definitions

A definition (denoted `def` in the syntax) declares a name with statically known data. This is used when renaming types or defining constants. Definitions are disambiguated by their name, their type, and their initial value. They may take up two distinct forms, a type definition, and a value definition. Type definitions do not include a value, and are considered distinct types. Value definitions are different in that they _must_ include a value, and are considered to be of the same type as the value, or as specified in the (for values) optional type.

```ebnf
typedef = `def` Name `:` TypeExpression `;`

constdef = `def` Name `:` TypeExpression? '=' Expression `;`
```

## Variables

A variable structure declares a name within the current scope, which carries with it a type and space in memory for a storage. All variable declarations are considered distinct, no matter the name, type, or value. Variable declarations also come in two syntactical forms, the first reserves space for a variable on the stack and initializes it to a default state. The second reserves the space, then copies the value of the expression into the variable.

In-place initialization of a variable requires a subsequent operation on the memory reserved by the declaration.

```ebnf
vardecl = Name `:` TypeExpression `;`

variable = Name `:` TypeExpression? `=` Expression `;`
```

## List Members

A list member may be referred to with two methods; indexing, and an optional name. When a name is bound to the list member, that member may be referred to using dot notation.
