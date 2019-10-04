
def Token = [
    type: TokenType
    span: SpannedText
    text: Key
]

TokenType = enum [
    Invalid, Eof, Eol = '\n',
    Name, Integer,
    Lparen = '(', Rparen = ')',
    Lbracket = '[', Rbracket = ']',
    Comma = ',', Dot = '.', Semicolon = ';',
    Plus = '+', Minus = '-', Slash = '/', Star = '*', Caret = '^',
    Equals = '=',
    Rarrow,
    If, Else, Loop, Break, Return,
    Let, Def
]

def Lexer = [
    source_text:    const(char)*
    end_of_text:    const(char)*
    next:           Token
    current:        Token
    eol_type_stack: Array(Type)
    table:          StringTable
    source:         SpannedText
]

/*
 name searching is done in the following order:

 1. within function
 2. within enclosing scopes (in order)
 3. if function, attached to first parameter
 */
def Lexer::attach = [
    new = (&self, source: const(Char)[]) -> {
        self.source_text = source.ptr
        self.end_of_text = source.ptr + source.length
        clear self.eol_type_stack
        advance self
    }
    push_eol_type = (&self, t: TokenType) -> push_back (t, self.eol_type_stack)
    pop_eol_type = (&self) -> pop_back self.eol_type_stack
    advance = (&self) -> {
        if next.type != TokenType.Eol {
            lex()
        }
        else {
            
        }
    }
}
