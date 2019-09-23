# Todo

## Current

- [ ] Implement chunking of invalid characters
  - [ ] Implement checking and reporting of UTF8 characters as names
- [ ] Elaborate syntax reporter error messages
- [ ] Implement parsing for named list members
- [ ] Refactoring/Refining pass on parser and lexer (2 hour limit!!!)

## List

- [ ] Lexical analysis
  - [x] Whitespace skipping
    - [x] Tested
  - [x] End-of-file tokens
    - [x] Tested
  - [x] End-of-line tokens
    - [ ] Tested
  - [x] Delimiters: [ ] ( ) , ;
    - [x] Tested
  - [x] Alphanumeric identifiers
    - [x] Tested
  - [x] Decimal numbers with underscore separator
    - [x] Token identification
      - [x] Tested
        - [x] Value parsing
          - [x] Tested
  - [ ] Chunked invalid characters
    - [ ] Invalid character identification
      - [ ] Tested
  - [x] Automatic delimiter (comma, semicolon) insertion
    - [ ] Tested
  - [ ] Profiled
- [ ] Syntactical analysis
  - [x] AST classes
  - [ ] Syntax reporter
    - [ ] Line-and-column finder
      - [ ] Tested
    - [x] AST printer
      - [x] Tested
    - [x] Error handlers (refer to syntax_errors.md)
      - [x] Tested
  - [ ] Parser
    - [ ] Expressions
      - [ ] Lists
        - [x] Unnamed members
          - [x] Tested
        - [ ] Named members
          - [ ] Tested
      - [x] Names
        - [x] Tested
      - [x] Integers
        - [x] Tested
      - [x] Binary Operators
        - [x] Negate
        - [x] Add
        - [x] Subtract
        - [x] Multiply
        - [x] Divide
        - [x] Power
        - [x] Call
        - [x] Tested
      - [ ] Function literals
      - [ ] Tested
    - [ ] Statements
      - [ ] If-else branching
      - [ ] While-loops
    - [ ] Profiled
