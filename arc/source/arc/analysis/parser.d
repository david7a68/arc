module arc.analysis.parser;

import arc.analysis.lexer;
import arc.data.ast;
import arc.data.scopes;
import arc.data.stringtable : StringTable;
import arc.data.symbol;
import arc.reporter : ArcError, Reporter, tprint;
import arc.source : Source;

interface IParser {
    // I seem to rewrite the parser every few months. To make my life a little
    // easier, the interface and implementation are split, so that I don't have
    // to make changes to anything that touches this ever single time.

    /**
        Parses a source text to produce an abstract syntax tree. Any identifiers
     that are encountered will be inserted into the string table, and any
     declarations inserted into the symbol table. As blocks and lists are
     encountered, a scope tree will be constructed in parallel to the syntax
     tree which will be accessible by every `AstNode`'s `outer_scope_id` member.


        If parse errors are encountered, the parser will attempt to partially
     recover and continue parsing in order to identify further parse errors up
     to an implementation-defined limit. However, the entire file will be marked
     invalid, and a single `Invalid` node will be returned. Any parse errors
     will be passed on to a `reporter` passed to the parser before the `parse()`
     function was called. It is an error for no `reporter` to be present.


        The memory managers `AstAllocator`, `ScopeAllocator`, and
     `GlobalSymbolTable` may assume that a parse action was successful and that
     any allocation will not be freed except when all of the manager's objects
     are no longer needed and the allocator itself is discarded.
     */
    SyntaxTree parse(Source* source);

    debug {
        /**
            A debug parse function that parses a single statement and then
         stops. All other considerations are identical to `parse()`.
         */
        SyntaxTree parse_stmt(Source* source);

        /**
            A debug parse function that parses a single expression and then
         stops. All other considerations are identical to `parse()`.
         */
        SyntaxTree parse_expr(Source* source);
    }
}

final class Parser : IParser {

    enum default_max_parse_errors = 5;
    enum token_buffer_size = 4096;

public:
    this(Reporter* reporter,
            StringTable* string_table,
            AstAllocator nodes,
            ScopeAllocator* scopes,
            GlobalSymbolTable* symbols) {
        this.reporter = reporter;
        this.token_buffer = new Token[](token_buffer_size);
        this.string_table = string_table;
        this.nodes = nodes;
        this.scopes = scopes;
        this.symbols = symbols;
    }

    override SyntaxTree parse(Source* source) {
        auto parser = ParserImpl(this, source.text);

        AstNodeId[] statements;

        size_t encountered_errors;
        while (encountered_errors < max_parse_errors && !parser.is_done) {
            auto stmt = parser.stmt();
            statements ~= stmt;

            if (!nodes.ast_of(stmt).is_valid)
                encountered_errors++;
        }

        return SyntaxTree(nodes, source, statements);
    }

    debug {
        override SyntaxTree parse_stmt(Source* source) {
           auto parser = ParserImpl(this, source.text);
           return SyntaxTree(nodes, source, [parser.stmt()]);
        }

        override SyntaxTree parse_expr(Source* source) {
           auto parser = ParserImpl(this, source.text);
           return SyntaxTree(nodes, source, [parser.expr()]);
        }
    }

private:
    Reporter* reporter;
    Token[] token_buffer;
    StringTable* string_table;
    AstAllocator nodes;
    ScopeAllocator* scopes;
    GlobalSymbolTable* symbols;
    uint max_parse_errors = default_max_parse_errors;
}

private:

alias TT = Token.Type;

/**
 The ParserImpl contains per-parse state as well as the actual parser
 implementation. This was done so that module state and per-action state can
 remain separate, making things a little cleaner.
 */
struct ParserImpl {
    import arc.data.hash : Hash;
    import arc.data.span : Span;

public:
    this(Parser parser, const(char)[] source) {
        _parser = parser;
        _tokens = TokenBuffer(source, parser.token_buffer, parser.string_table);
        _scope_id = _parser.scopes.make_scope(_parser.scopes.null_scope_id);
    }

    bool is_done() {
        return token.type == TT.Done;
    }

    AstNodeId stmt() {
        auto stmt = () {
            switch (token.type) with (TT) {
            case Lbrace:
                return block();
            case TokLoop:
                return seq!(Loop, stmt, Lbrace, Rbrace)(take(required(TokLoop)).span);
            case TokBreak:
                return alloc(take.span, Break(_scope_id));
            case TokContinue:
                return alloc(take.span, Continue(_scope_id));
            case TokReturn:
                return alloc(take().span, Return(_scope_id, is_at_end_of_expr ? none : expr()));
            case TokIf:
                return alloc(take().span, If(_scope_id, expr(), block(), skip(token.type == TokElse) ? stmt() : none));
            case TokDef:
                return decl!Definition(take().span, Symbol.Kind.Constant, take(required(TT.TokName)));
            default:
                if (token.type == TokName && next.type == Colon)
                    return decl!Variable(token.span, Symbol.Kind.Variable, take());

                auto lhs = expr();
                if (skip(token.type == Equals)) {
                    return alloc(_parser.nodes.span_of(lhs), BinOp(AstNode.Kind.Assign, _scope_id, lhs, expr()));
                }
                return lhs;
            }
        }();

        switch (_parser.nodes.ast_of(stmt).kind) with (AstNode.Kind) {
        case Block:
        case If:
        case Loop:
        case Invalid:
            return stmt;
        default:
            const end = token;
            if (take(required(TT.Semicolon))) {
                _parser.nodes.span_of(stmt) += end.span;
                return stmt;
            }

            return alloc(_parser.nodes.span_of(stmt), arc.data.ast.Invalid(_scope_id));
        }
    }

    AstNodeId expr(Precedence p = Precedence.Assign) {
        return infix_expr(p, prefix());
    }

private:
    // dfmt off
    AstNodeId none() { return _parser.nodes.none; }
    // dfmt on

    alias block = seq!(Block, stmt, TT.Lbrace, TT.Rbrace);

    /**
     <symbol> ':' (<expr> | <inferred_type>) (('=' <expr>) | <none>)
     */
    AstNodeId decl(Node)(Span prefix, Symbol.Kind kind, Token name) {
        assert(name.type == TT.TokName);

        skip(required(TT.Colon));

        auto symbol = _parser.symbols.make_symbol(kind, name.key);
        _parser.scopes.scope_of(_scope_id).add(symbol);

        return alloc!Node(prefix, Node(
                _scope_id,
                symbol,
                token.type != TT.Equals ? expr() : inferred_type(),
                skip(token.type == TT.Equals) ? expr(Precedence.Logic) : none));
    }

    bool is_at_end_of_expr() {
        with (TT)
            return is_done || token.type.matches_one(Semicolon, Comma, Rparen, Rbracket);
    }

    AstNodeId unary(AstNode.Kind kind) {
        auto span = take().span;
        auto e = expr(Precedence.Call);

        if (_parser.nodes.ast_of(e).is_valid)
            return alloc(span, UnOp(kind, _scope_id, e));

        return alloc(span, Invalid(_scope_id));
    }

    AstNodeId prefix() {
        // dfmt off
        switch (token.type) with (TT) {
        case Bang:          return unary(AstNode.Kind.Not);
        case TokNot:        return unary(AstNode.Kind.Not);
        case Minus:         return unary(AstNode.Kind.Negate);
        case Star:          return unary(AstNode.Kind.Dereference);
        case TokImport:     return unary(AstNode.Kind.Import);
        case TokName:       return alloc(token.span, SymbolRef(_scope_id, take().key));
        case TokInteger:    return alloc(token.span, Integer(_scope_id, take().value));
        case TokString:     return alloc(token.span, String(_scope_id, take().key));
        case TokChar:       return alloc(token.span, Char(_scope_id, take().key));
        case Lparen:        return list!(Lparen, Rparen)();
        case Lbracket:      return list!(Lbracket, Rbracket)();
        default:
            report_expect_mismatch(token,
                is_at_end_of_expr
                ? "The expression ended unexpectedly with %s."
                : "The token \"%s\" cannot start a prefix expression",
                token.type);
            return alloc(token.span, arc.data.ast.Invalid(_scope_id));
        }
        // dfmt on
    }

    AstNodeId infix(alias fn, Infix[] ops)(Precedence p, AstNodeId lhs) {
        for (Infix op = ops[token.type]; _parser.nodes.ast_of(lhs).is_valid && p <= op.prec; op = ops[token.type]) {
            skip(op.skip_token);
            auto rhs = fn(cast(Precedence)(op.prec + op.is_left_associative));
            auto span = _parser.nodes.span_of(lhs) + _parser.nodes.span_of(rhs);
            lhs = alloc(span, BinOp(op.kind, _scope_id, lhs, rhs));
        }
        return lhs;
    }

    alias infix_expr = infix!(expr, binops);

    AstNodeId seq(Node, alias member, TT lhs, TT rhs, TT delim = TT.None)(Span prefix = Span()) {
        auto start = take(required(lhs)).span;

        auto outer_scope = _scope_id;
        _scope_id = _parser.scopes.make_scope(_scope_id);
        scope (exit)
            _scope_id = outer_scope;

        AstNodeId[] array;
        while (!is_done && token.type != rhs) {
            auto node = member();

            const is_bad = delim && !(is_done || token.type == rhs || skip(required(delim)));
            if (!_parser.nodes.ast_of(node).is_valid || is_bad) {
                destroy(array);
                return alloc(start, Invalid(outer_scope));
            }

            while (token.type == delim)
                advance();
            array ~= node;
        }

        const span = prefix + token.span + start;
        if (skip(required(rhs)))
            return alloc(span, Node(outer_scope, _scope_id, array));

        destroy(array);
        return alloc(span, Invalid(outer_scope));
    }

    AstNodeId list_member() {
        AstNodeId lm(Span span, AstNodeId type, AstNodeId expr) {
            if (_parser.nodes.ast_of(type).is_valid && _parser.nodes.ast_of(expr).is_valid)
                return alloc(span, ListMember(_scope_id, _parser.symbols.none, type, expr));
            return alloc(span, Invalid(_scope_id));
        }

        // (a : T = 1)
        if (token.type == TT.TokName && next.type == TT.Colon)
            return decl!ListMember(token.span, Symbol.Kind.Variable, take());

        // (a)
        auto e = expr();
        if (_parser.nodes.ast_of(e).is_valid)
            return alloc(_parser.nodes.span_of(e), ListMember(_scope_id, _parser.symbols.none, inferred_type(), e));
        return alloc(_parser.nodes.span_of(e), Invalid(_scope_id));
    }

    AstNodeId list(TT open, TT close)() {
        auto node = seq!(List, list_member, open, close, TT.Comma)();
        if (token.type == TT.RArrow)
            return function_type(node);
        else if (token.type == TT.RFatArrow)
            return function_(node);
        return node;
    }

    /*
    Arc has three forms of function literals:
        a =>             <expr>;
        a => <type_expr> { ... };
        a =>             { ... };
    */
    AstNodeId function_(AstNodeId params) {
        skip(required(TT.RFatArrow));

        // a => {}
        if (token.type == TT.Lbrace)
            return alloc(token.span, Function(_scope_id, params, inferred_type(), block()));

        auto perhaps_body = expr();

        // a => t {}
        if (token.type == TT.Lbrace)
            return alloc(_parser.nodes.span_of(params), Function(_scope_id, params, perhaps_body, block()));

        // a => b
        return alloc(_parser.nodes.span_of(params), Function(_scope_id, params, inferred_type(), perhaps_body));
    }

    // () -> RetType
    AstNodeId function_type(AstNodeId params) {
        skip(required(TT.RArrow));
        return alloc(_parser.nodes.span_of(params), FunctionSignature(_scope_id, params, expr()));
    }

    alias _tokens this;

    Token take(bool should_advance = true) {
        auto t = token;

        if (should_advance)
            advance();

        return t;
    }

    bool skip(bool should_skip) {
        if (should_skip)
            advance();
        return should_skip;
    }

    bool required(TT type) {
        if (type == token.type)
            return true;

        report_expect_mismatch(
                token,
                "An unexpected token was encountered. Expected a %s, but got a %s instead",
                type, token.type);

        return false;
    }

    AstNodeId alloc(T)(Span location, T node) {
        if (node.is_valid)
            return _parser.nodes.save_node(location, node);
        else
            return _parser.nodes.save_node(location, Invalid(node.outer_scope_id));
    }

    AstNodeId alloc(T : Invalid)(Span location, T node) {
        return _parser.nodes.save_node!T(location, node);
    }

    AstNodeId inferred_type() {
        return alloc(Span(), InferredType(_scope_id));
    }

    void report_expect_mismatch(Args...)(Token got, string message, Args args) {
        if (got.type == TT.Done)
            _parser.reporter.error(ArcError.UnexpectedEOF, got.span, "Unexpected end of file");
        else
            _parser.reporter.error(ArcError.TokenExpectMismatch, got.span, message, args);
    }

    Parser _parser;
    ScopeId _scope_id;
    TokenBuffer _tokens;
}

enum Precedence : ubyte {
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

// dfmt off
immutable Infix[256] binops = [
    TT.TokAnd       : Infix(Precedence.Logic,    true,  true,  AstNode.Kind.And),
    TT.TokOr        : Infix(Precedence.Logic,    true,  true,  AstNode.Kind.Or),
    TT.EqualEqual   : Infix(Precedence.Equality, true,  true,  AstNode.Kind.Equal),
    TT.BangEqual    : Infix(Precedence.Equality, true,  true,  AstNode.Kind.NotEqual),
    TT.Less         : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.Less),
    TT.LessEqual    : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.LessEqual),
    TT.Greater      : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.Greater),
    TT.GreaterEqual : Infix(Precedence.Compare,  true,  true,  AstNode.Kind.GreaterEqual),
    TT.Plus         : Infix(Precedence.Sum,      true,  true,  AstNode.Kind.Add),
    TT.Minus        : Infix(Precedence.Sum,      true,  true,  AstNode.Kind.Subtract),
    TT.Star         : Infix(Precedence.Product,  true,  true,  AstNode.Kind.Multiply),
    TT.Slash        : Infix(Precedence.Product,  true,  true,  AstNode.Kind.Divide),
    TT.Caret        : Infix(Precedence.Power,    false, true,  AstNode.Kind.Power),
    TT.Dot          : Infix(Precedence.Call,     true,  true,  AstNode.Kind.Access),
    TT.ColonColon   : Infix(Precedence.Call,     true,  true,  AstNode.Kind.StaticAccess),
    TT.Lparen       : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
    TT.Lbracket     : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
    TT.TokName      : Infix(Precedence.Call,     false,  false, AstNode.Kind.Call),
    TT.TokInteger   : Infix(Precedence.Call,     false,  false, AstNode.Kind.Call),
    TT.TokChar      : Infix(Precedence.Call,     false,  false, AstNode.Kind.Call),
    TT.TokString    : Infix(Precedence.Call,     false,  false, AstNode.Kind.Call),
];

immutable Infix[256] type_ops = [
    TT.Dot          : Infix(Precedence.Call,     true,  true,  AstNode.Kind.Access),
    TT.Lparen       : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
    TT.Lbracket     : Infix(Precedence.Call,     true,  false, AstNode.Kind.Call),
];
// dfmt on
