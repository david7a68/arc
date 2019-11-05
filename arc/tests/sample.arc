def Parser := [
    lexer: Lexer
    table: *StringTable
    error: *SyntaxReporter
]

def make_parser := (source: SpannedText, table: *StringTable, error: *SyntaxReporter) => {
    p := [make_lexer(source, table), table, error]
    read(p.lexer)
}

def skip := (p: *Parser, t: TokenType) => {
    if type != p.current.type { return false }

    advance(p.lexer)
    return true
}

def matches := (T: Type) => (t: T, types: T[]) => {
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
    push_eol_type(p.lexer, Tok_Semicolon)
    type := p.lexer.current.type

    s := if type == Tok_Def {
        def_(p)
    } else if type == Tok_Break {
        break_(p)
    } else if type == Tok_Return {
        return_(p)
    } else if type == Tok_Continue {
        continue_(p)
    } else expression(p)

    consume(p, Tok_Semicolon)
    pop_eol_type(p.lexer)
    s
}

def def_ := (p: *Parser) => {
    span := p.lexer.current.span
    consume(p, Tok_Def)

    name := name(p)
    loop { if p.lexer.current.type == Tok_ColonColon {
        name = path(p, name)
    } else {
        break
    }}

    type := AstNode.none
    if p.consume(Tok_Colon) == false {
        p.error.definition_missing_colon(merge(span, name.span), p.current.span)
        type := make_invalid(new_span(0, 0))
    } else if p.lexer.current.type != Tok_Equals {
        type = primary(p)
    }

    consume(p, Tok_Equals)
    value := expression(p)

    make_n_ary(Ast_Define, merge(span, value.span), name, type, value)
}
