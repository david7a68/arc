module arc.analysis.parser;

import arc.analysis.lexer : matches_one, Token, TokenBuffer;
import arc.data.ast;
import arc.data.hash : Hash;
import arc.data.span : Span;
import arc.data.symbol : Symbol, SymbolTable;
import arc.memory : ArrayPool, VirtualMemory;
import arc.reporter : ArcError, Reporter, tprint;

struct ParseUnit {
    VirtualMemory* vm;
    ArrayPool!(AstNode*)* arrays;
    Reporter* reporter;
    SymbolTable* global_symbol_table;
    const(char)[] source;
}

struct Parser {
    enum token_buffer_size = 4096;

public:
    TokenBuffer!token_buffer_size tokens;

    ParseUnit unit;
    alias unit this;

    void begin(ParseUnit unit) {
        this.unit = unit;
        tokens.begin(unit.source);
        global_symbol_table.push_scope();
    }

    void end() {
        global_symbol_table.pop_scope();
    }

    // dfmt off
    Token token() { return tokens.current; }

    bool is_done() { return token.type == Token.Type.Done; }
    // dfmt on

    AstNode*[] parse(ParseUnit unit) {
        begin(unit);
        auto statements = arrays.get_appender();
        size_t num_errors;
        while (!is_done && num_errors < 5) {
            auto statement = stmt();
            statements ~= statement;

            if (!statement.is_valid)
                num_errors++;
        }
        end();
        return statements.get();
    }

    AstNode* stmt() {
        auto stmt = () {
            switch (token.type) with (Token.Type) {
            case TokIf:
                return alloc!If(take().span, expr(), block(), skip(token.type == TokElse) ? stmt() : none);

            // dfmt off
            case Lbrace:        return block();
            case TokBreak:      return alloc!Break(take().span);
            case TokContinue:   return alloc!Continue(take().span);
            case TokDef:        return decl(AstNode.Kind.Definition, take.span, take.key);
            case TokReturn:     return alloc!Return(take().span, is_end_of_expr ? none : expr());
            case TokLoop:       return seq!(Loop, stmt, Lbrace, Rbrace)(take(required(TokLoop)).span);
            // dfmt on

            default:
                AstNode* e;
                if (token.type == TokName) {
                    auto first = take();
                    if (token.type == Colon)
                        return decl(AstNode.Kind.Variable, first.span, first.key);
                    e = infix_expr(Precedence.Assign, alloc!SymbolRef(first.span, first.key));
                }
                else
                    e = expr();

                if (token.type == Equals) {
                    advance();
                    return alloc!BinOp(AstNode.Kind.Assign, e, expr());
                }
                return e;
            }
        }();

        switch (stmt.kind) {
        case AstNode.Kind.Block:
        case AstNode.Kind.If:
        case AstNode.Kind.Loop:
        case AstNode.Kind.Invalid:
            return stmt;

        default:
            if (auto end = take(required(Token.Type.Semicolon))) {
                stmt.span += end.span;
                return stmt;
            }

            return alloc!Invalid(stmt.span);
        }
    }

    AstNode* expr(Precedence p = Precedence.Assign) {
        return infix_expr(p, prefix());
    }

    AstNode* type(Precedence p = Precedence.Assign) {
        return infix_type(p, type_prefix());
    }

private:
    alias block = seq!(Block, stmt, Token.Type.Lbrace, Token.Type.Rbrace);

    AstNode* decl(AstNode.Kind kind, Span prefix, Hash name) {
        skip(required(Token.Type.Colon));
        const sym_kind = kind == AstNode.Kind.Definition ? Symbol.Kind.Constant : Symbol.Kind.Variable;
        // dfmt off
        return alloc!Declaration(kind, prefix,
                global_symbol_table.make_symbol(sym_kind, name),
                token.type != Token.Type.Equals ? type() : inferred,
                skip(token.type == Token.Type.Equals) ? expr(Precedence.Logic) : inferred);
        // dfmt on
    }

    alias infix_expr = infix!(expr, binops);

    // dfmt off
    immutable Infix[256] binops = [
        Token.Type.Less        : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.Less),
        Token.Type.LessEqual   : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.LessEqual),
        Token.Type.Greater     : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.Greater),
        Token.Type.GreaterEqual: Infix(Precedence.Compare,  true,  true,  AstNode.Kind.GreaterEqual),
        Token.Type.EqualEqual  : Infix(Precedence.Equality, true,  true,  AstNode.Kind.Equal),
        Token.Type.BangEqual   : Infix(Precedence.Equality, true,  true,  AstNode.Kind.NotEqual),
        Token.Type.TokAnd      : Infix(Precedence.Logic,    true,  true,  AstNode.Kind.And),
        Token.Type.TokOr       : Infix(Precedence.Logic,    true,  true,  AstNode.Kind.Or),
        Token.Type.Plus        : Infix(Precedence.Sum,      true,  true,  AstNode.Kind.Add),
        Token.Type.Minus       : Infix(Precedence.Sum,      true,  true,  AstNode.Kind.Subtract),
        Token.Type.Star        : Infix(Precedence.Product,  true,  true,  AstNode.Kind.Multiply),
        Token.Type.Slash       : Infix(Precedence.Product,  true,  true,  AstNode.Kind.Divide),
        Token.Type.Caret       : Infix(Precedence.Power,    false, true,  AstNode.Kind.Power),
        Token.Type.Dot         : Infix(Precedence.Call,     true,  true,  AstNode.Kind.Access),
        Token.Type.ColonColon  : Infix(Precedence.Call,     true,  true,  AstNode.Kind.StaticAccess),
        Token.Type.TokName     : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
        Token.Type.TokInteger  : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
        Token.Type.TokChar     : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
        Token.Type.TokString   : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
        Token.Type.Lparen      : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
        Token.Type.Lbracket    : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
    ];
    // dfmt on

    AstNode* infix(alias fn, Infix[] ops)(Precedence p, AstNode* lhs) {
        for (Infix op = ops[token.type]; lhs.is_valid && p <= op.prec; op = ops[token.type]) {
            skip(op.skip_token);
            lhs = alloc!BinOp(op.kind, lhs, fn(cast(Precedence)(op.prec + op.is_left_associative)));
        }
        return lhs;
    }

    AstNode* prefix() {
        // dfmt off
        switch (token.type) with (Token.Type) {
        case Bang:
        case TokNot:        return alloc!UnOp(AstNode.Kind.Not, take().span, expr(Precedence.Call));
        case Minus:         return alloc!UnOp(AstNode.Kind.Negate, take().span, expr(Precedence.Call));
        case TokImport:     return alloc!Import(take().span, expr(Precedence.Call));
        case TokName:       return alloc!SymbolRef(token.span, take().key);
        case TokInteger:    return alloc!IntLiteral(token.span, take().value);
        case TokString:     return alloc!StrLiteral(token.span, take().key);
        case TokChar:       return alloc!CharLiteral(take.span, '\0');
        case Lparen:        return list!(Lparen, Rparen)();
        case Lbracket:      return list!(Lbracket, Rbracket)();
        default:
        }

        if (is_done)
            reporter.error(ArcError.UnexpectedEOF, token.span, "Unexpected end of file while parsing expression.");
        else
            reporter.error(ArcError.TokenExpectMismatch, token.span,
                is_end_of_expr
                ? tprint("Expression ended unexpectedly by terminating %s.", token.type)
                : tprint("The token \"%s\" cannot start a prefix expression", token.type));
        // dfmt on
        return alloc!Invalid(is_end_of_expr ? token.span : take.span);
    }

    AstNode* list(Token.Type open, Token.Type close)() {
        auto node = seq!(List, list_member!false, open, close, Token.Type.Comma)();
        return token.type == Token.Type.Rarrow ? function_(node) : node;
    }

    AstNode* list_member(bool is_type)() {
        auto first = prefix();
        if (first.kind == AstNode.Kind.SymbolRef && token.type == Token.Type.Colon) {
            const s = first.span;
            const k = (cast(SymbolRef*) first).text;
            vm.free_to_ptr(first);
            return decl(AstNode.Kind.ListMember, s, k);
        }

        static if (is_type)
            return alloc!Declaration(AstNode.Kind.ListMember, infix_type(Precedence.Assign, first), inferred);
        else
            return alloc!Declaration(AstNode.Kind.ListMember, inferred, infix_expr(Precedence.Assign, first));
    }

    AstNode* function_(AstNode* node) {
        advance();
        auto e = token.type == Token.Type.Lbrace ? inferred : expr();
        return token.type == Token.Type.Lbrace
            ? alloc!Function(node, e, block()) : alloc!Function(node, inferred, e);
    }

    alias infix_type = infix!(type, type_ops);

    // dfmt off
    immutable Infix[256] type_ops = [
        Token.Type.Dot         : Infix(Precedence.Call,     true,  true,  AstNode.Kind.Access),
        Token.Type.Lparen      : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
        Token.Type.Lbracket    : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
    ];
    // dfmt on

    AstNode* type_prefix() {
        // dfmt off
        switch (token.type) with (Token.Type) {
        case Lparen:    return type_list!(Lparen, Rparen)();
        case Lbracket:  return type_list!(Lbracket, Rbracket)();
        case TokName:   return alloc!SymbolRef(token.span, take.key);
        case Star:      return alloc!UnOp(AstNode.Kind.PointerType, take().span, type());
        default:
        }
        // dfmt on

        reporter.error(ArcError.TokenExpectMismatch, token.span, "A type expression cannot start with %ss", token.type);
        return alloc!Invalid(take().span);
    }

    AstNode* type_list(Token.Type open, Token.Type close)() {
        auto node = seq!(List, list_member!true, open, close, Token.Type.Comma)();
        return token.type == Token.Type.Rarrow ? function_type(node) : node;
    }

    AstNode* function_type(AstNode* params) {
        advance();
        return alloc!FunctionSignature(params, type());
    }

    AstNode* alloc(T, Args...)(Args args) {
        auto t = T(args);
        return verify(t.header) ? raw_alloc!T(t) : alloc!Invalid(t.span);
    }

    AstNode* alloc(T : Invalid)(Span span) {
        return raw_alloc(Invalid(span));
    }

    AstNode* raw_alloc(T)(T t) {
        auto n = vm.alloc!T;
        *n = t;
        n.enclosing_scope = global_symbol_table.current_scope;
        return n.header;
    }

    void advance() {
        tokens.advance();
    }

    Token take(bool should_take = true) {
        if (!should_take)
            return Token();

        auto t = token;
        advance();
        return t;
    }

    bool skip(bool should_skip) {
        if (should_skip)
            advance();
        return should_skip;
    }

    bool required(Token.Type type) {
        if (type == token.type)
            return true;

        const span = token.span;
        if (is_done)
            reporter.error(ArcError.UnexpectedEOF, span,
                    "The file ended unexpectedly. A %s was expected.", type);
        else
            reporter.error(ArcError.TokenExpectMismatch, span,
                    "An unexpected token was encountered: Expected (%s), Encountered (%s)",
                    type, source[span.start .. span.start + span.length]);
        return false;
    }

    bool is_end_of_expr() {
        with (Token.Type)
            return is_done || token.type.matches_one(Semicolon, Comma, Rparen, Rbracket);
    }

    AstNode* seq(T, alias member, Token.Type open, Token.Type close, Token.Type delim = Token.Type.None)(Span prefix = Span()) {
        auto start = take(required(open)).span;
        auto seq = arrays.get_appender();
        auto scope_ = global_symbol_table.push_scope();
        scope (exit)
            global_symbol_table.pop_scope();

        while (!is_done && token.type != close) {
            auto node = member();

            const is_bad = delim && !(is_done || token.type == close || skip(required(delim)));
            if (!node.is_valid || is_bad) {
                seq.abort();
                return alloc!Invalid(start + node.span);
            }

            while (token.type == delim)
                advance();

            seq ~= node;
        }

        auto span = prefix + token.span + start;
        if (skip(required(close)))
            return alloc!T(span, scope_, seq.get());

        seq.abort();
        return alloc!Invalid(span);
    }
}

private:
enum Precedence: ubyte {
    None,
    Assign,
    Logic,
    Equality,
    Compare,
    Sum,
    Product,
    Power,
    Call,
    Prefix
}

struct Infix {
    Precedence prec;
    bool is_left_associative, skip_token;
    AstNode.Kind kind;
}
