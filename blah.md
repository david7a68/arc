# Blah

A 'simple' programming language.

## The Ideal Programming Language

* Simple and expressive but easily comprehensible grammar
* Atomic, composable features
* Low-effort C header integration
* Operating system compatibility modules
* Small, useful standard library
* CTFE
* Flexible, formally verified type system
* Robust tooling support (compiler, editor, documentation generator, etc)

## Data Types

* Integer
* Character (aka ubyte)
* List
* Function
* Namespaces

### Operations on Integers

* Add: `a + b`
* Subtract: `a - b`
* Multiply: `a * b`
* Divide: `a / b`
* Modulus: `a % b`
* Identity: `3 == 4 -> False`
* Compare: `< > <= >=`
* Range: `0 .. 3`

### Operations on Lists

* Duplicative
  * Join: `(1, 2) ~ (1, 3) -> (1, 2, 1, 3)`
  * Instantiation: `(Int, Int, Bool) (0, 0, True) -> (0, 0, True)`
* Non-Duplicative
  * Slice: `(a, b, c, d) 3 .. 1 -> (d*, 1)`, `(a, b, c, d)() -> (a*, 4)`
  * Index: `(i, j, k) 2 -> *k`
  * Identity: `(0: Int) == (0: Int 0 .. 10) -> False`
  * Decompose: `(False) -> False`, `(1) -> 1`

### Operations on Functions

* Call: `F x`
* Define: `F := x : Int -> x * x`
* Define: `F : Int -> Int = x -> x * x`

### Operations on Namespaces

* Access: `N::F -> F`
* Append: `N::T := V`, `N::{T := V; K := P}`
* Construction: `{T := V; K := P}`
* Scoped Union: `with (import std::console) { ... }`

## Samples

```arc
It : (
    buffer : U8[],
    cursor : Uint,
    end : Uint
)::{
    construct := data: U8[] -> (data, 0, data.length);
    is_done := () -> cursor == end;
    current := () -> buffer cursor;
    advance := () -> cursor++;
}

main := args: String[] -> {
  for v in It (std::file::read "example.arc") {
      std::console::writeln(v)
  }
}
```

```arc
exit : extern C () -> ();
```
