# Todo

## Current

- [ ] Implement chunking of invalid characters
  - [ ] Implement checking and reporting of UTF8 characters as names
  - [ ] Parse invalid tokens into invalid nodes

## List

- [ ] Lexical analysis
  - [x] Whitespace skipping
    - [x] Tested
  - [x] End-of-file tokens
    - [x] Tested
  - [x] End-of-line tokens
    - [x] Tested
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
    - [x] Tested
- [x] Syntactical analysis
  - [x] AST classes
  - [x] Syntax reporter
    - [x] Line-and-column finder
      - [x] Tested
    - [x] AST printer
      - [x] Tested
    - [x] Error handlers (refer to syntax_errors.md)
      - [x] Tested
  - [x] Parser
    - [x] Expressions
      - [x] Lists
        - [x] Unnamed members
          - [x] Tested
        - [x] Named members
          - [x] Tested
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
      - [x] Function literals
      - [x] Tested
    - [x] Statements
      - [x] If-else branching
      - [x] Loops
    - [x] Declarations
      - [x] Def declarations
  - [ ] Semantic Definitions
    - [ ] Partial tuple initialization
