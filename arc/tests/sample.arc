def Parser := [
    lexer: Lexer
    table: *StringTable
    error: *SyntaxReporter
]

def Parser::new := (source: SpannedText, table: *StringTable, error: *SyntaxReporter) => {
    p := [Lexer::new(source, table), table, error]
    p.lexer.read()
}

def Parser::consume := (self, t: Token::Type) => {
    if type != self.current.type { return false }

    self.advance()
    return true
}

def Parser::empty := (self) => self.current.type == Token::Eof

def matches := (T: meta::Type) => (t: T, types: T[]) => {
    i := 0
    loop { if i < types.length {
        if types[i] == t { return true }
    } else {
        break
    }}
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

def def_ := (p: *Parser) => {
    span := p.lexer.current.span
    p.consume(Token::Def)

    name := name(p)
    loop { if p.lexer.current.type == Token::ColonColon {
        name = path(p, name)
    } else {
        break
    }}

    type := AstNode.none
    if p.consume(Token::Colon) == false {
        p.error.definition_missing_colon(span.merge(name.span), p.current.span)
        type := make_invalid(Span::new(0, 0))
    } else if p.lexer.current.type != Token::Equals {
        type = primary(p)
    }

    p.consume(Token::Equals)
    value := expression(p)

    make_n_ary(AstNode::Define, span.merge(value.span), name, type, value)
}
