// module arc::syntax::parser

// import arc::syntax::ast
// import arc::syntax::lexer
// import arc::syntax::location::{SpannedText, Span}}
// import arc::syntax::reporter::SyntaxReporter
// import arc::stringtable::StringTable

def Parser := [
    lexer: Lexer
    table: *StringTable
    error: *SyntaxReporter
]

def Parser::new := (source: SpannedText, table: *StringTable, error: *SyntaxReporter) => {
    p := [Lexer::new(source, table), table, error]
    p.lexer.read()
}

def Parser::consume := (self, Token::Type t) => {
    if type != self.current.type { return false }

    self.advance()
    return true
}

def Parser::empty := (self) => self.current.type == Token::Eof

def matches := (meta::Type T) => (t: T, T[] types) => {
    i := 0
    loop if i < types.length {
        if types[i] == t { return true }
    } else break
    return false
}


//-----------------------------------//
//            Statements             //
//-----------------------------------//


def statement := (p: *Parser) => {
    p.lexer.push_eol_type(Token::Semicolon)
    type := p.lexer.current.type

    s := if type == Token::Def {
        def_(p)
    } else if type == Token::Break {
        break_(p)
    } else if type == Token::Return {
        return_(p)
    } else if type == Token::Continue {
        continue_(p)
    } else expression(p)

    p.consume(Token::Semicolon)
    p.lexer.pop_eol_type()
    s
}
