# Compiler Architecture

## Considerations

* A compiler has a minimum complexity bounded by the language specification.
* The compiler should impose reasonable limits on program size and complexity.
  * Is 4 billion AST nodes sufficient for a source tree? That's ~128 Gib of data.
  * Using these short 'pointers' requires a single offset pointer

## Stuff

* Memory in the Arc compiler is split into contiguous chunks of virtual address space.
  * Generally, each stage has a chunk proportional to the amount of data it generates.
    * Syntax Analysis: 512 Gib between lexing and constructing the syntax tree.
    * Semantic Analysis: 1024 Gib for type checking, flow analysis, etc.
    * Lowering to Near-Assembly IR: 512 Gib.
  * Memory regions may be further subdivided to serve particular needs.
* Arc compiles everything in a project at once. Caching of partially built files is optional and an implementation detail.
* In the general case, memory consumption is not a concern (as long as it's not crazy).
  * Where possible, 0-allocation or fixed-allocation algorithms are preferred.
  
Source -> Tokens -> AST -> Annotated AST -> IR -> Output
  1x       0.1x     1x         5x           1x     10x
