# Todo

## Current

- [ ] Automatic delimiter insertion
  - [ ] Lexer does not yet produce EOL tokens

## List

- [ ] Lexical analysis
  - [x] Whitespace skipping
    - [x] Tested
  - [x] End-of-file tokens
    - [x] Tested
  - [x] Delimiters: [ ] ( ) ,
    - [x] Tested
  - [x] Alphanumeric identifiers
    - [x] Tested
  - [ ] Decimal numbers with underscore separator
    - [x] Token identification
      - [x] Tested
        - [ ] Value parsing
          - [ ] Tested
  - [ ] Chunked invalid characters
    - [ ] Invalid character identification
      - [ ] Tested
  - [ ] Profiled
- [ ] Syntactical analysis
  - [x] AST struct
  - [ ] Pooling allocator for AST nodes
    - [x] Allocate
      - [x] Tested
    - [x] Free
      - [x] Tested
    - [x] FreeChildren
      - [x] Tested
    - [x] FreeOnly
      - [x] Tested
    - [x] Clear
      - [x] Tested
    - [ ] Out of memory handling
      - [ ] Tested
    - [ ] Profiled
  - [ ] Syntax reporter
    - [ ] Line-and-column finder
      - [ ] Tested
    - [ ] AST printer
      - [ ] Tested
    - [ ] Error handlers (refer to syntax_errors.md)
      - [ ] Tested
  - [ ] Parser
    - [ ] Automatic delimiter (comma, semicolon) insertion
      - [ ] Tested
    - [x] Tuples
      - [x] Tested
    - [x] Vectors
      - [x] Tested
    - [ ] Names
      - [ ] Tested
    - [ ] Integers
      - [ ] Tested
    - [ ] Profiled
