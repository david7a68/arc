
def Token = [
    type: TokenType
    span: SpannedText
    text: Key
]

def TokenType = enum [
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
    source_text:    const(char)
    end_of_text:    const(char)
    next:           Token
    current:        Token
    eol_type_stack: Array(Type)
    table:          StringTable
    source:         SpannedText
]

Lexer => {
    new := (source: SpannedText, table: StringTable) => Lexer [
        .source_text = source.text.ptr
        .end_of_ext = source.text.ptr + source.length
        .table = table
        .source = source
    ]

    ready := (self) => {
        self.advance()
        self.advance()
    }

    push_eol_type := (self, t: TokenType) => eol_type_stack.insertBack(t)

    pop_eol_type := (self) => eol_type_stack.removeBack()

    advance := (self) => {
    }
}