module arc.syntax.syntax_reporter;

import std.format: format;
import std.stdio: writeln;
import arc.syntax.location;
import arc.hash: Key;
import arc.syntax.lexer: Token;
import arc.syntax.ast: AstNode;

struct SyntaxReporter {
    void* user_data;
    Source source;

    static with_default_handlers(void* data, Source source) {
        auto r = SyntaxReporter(data, source);
        r.set_default_handlers();
        return r;
    }

    /**
     * Quality of life dispatcher that lets us pretend that the error handlers
     * are methods.
     *
     * To call an error reporting function, such as tuple_missing_comma, simply
     * call `reporter.tuple_missing_comma(expr, loc)`. The reporter parameter
     * will be provided automatically.
     */
    template opDispatch(string s) {
        void opDispatch(Args...)(Args args) {
            mixin(s ~ "_impl(&this, args);");
        }
    }

    typeof(unexpected_end_of_file_error)*       unexpected_end_of_file_impl;
    typeof(token_type_mismatch_error)*          token_type_mismatch_impl;
    typeof(definition_missing_colon_error)*     definition_missing_colon_impl;
    typeof(seq_not_closed_error)*               seq_not_closed_impl;
    typeof(token_cannot_start_expr_error)*      token_cannot_start_expr_impl;
    typeof(token_is_not_an_operator_error)*    token_is_not_an_operator_impl;

    void set_default_handlers() {
        static string handler(string name) {
            return name ~ "_impl = &" ~ name ~ "_error;";
        }

        mixin(handler("unexpected_end_of_file"));
        mixin(handler("token_type_mismatch"));
        mixin(handler("definition_missing_colon"));
        mixin(handler("seq_not_closed"));
        mixin(handler("token_cannot_start_expr"));
        mixin(handler("token_is_not_an_operator"));
    }
}

struct SourceLoc {
    uint line;
    uint line_position;
    uint column;
}

SourceLoc get_loc(SpannedText source, CharPos position) {
    uint line_num = 1;
    uint line_pos;
    uint column;

    const slice = source.text[0 .. position - source.start];
    foreach (i, c; slice) {
        if (c == '\n') {
            column = 0;
            line_num++;
            line_pos = cast(uint) i;
        }
        column++;
    }

    return SourceLoc(line_num, line_pos, column);
}

void unexpected_end_of_file_error(SyntaxReporter* reporter) {
    writeln("Unexpected end of file");
}

void definition_missing_colon_error(SyntaxReporter* reporter, Span pos) {
    auto err_pos = reporter.source.get_loc(pos.start);
    const s = "Error: The definition at (line: %s, column: %s) must have a type specification.".format(
        err_pos.line,
        err_pos.column
    );
    writeln(s);
    writeln("    To automatically infer the type specification, insert a colon (:) after the name.");
    writeln("        Grammar: 'def' path ':' primary? '=' expr ';' ;");
}

void seq_not_closed_error(SyntaxReporter* reporter, Span expr_loc, Span err_loc, AstNode.Type type) {
    auto exp_loc = reporter.source.get_loc(expr_loc.start);
    auto err_pos = reporter.source.get_loc(err_loc.start);
    const s = "Error: The %s at (line: %s, column: %s) is not closed:\n%s (line: %s, column: %s)".format(
        type,
        exp_loc.line,
        exp_loc.column,
        reporter.source.name,
        err_pos.line,
        err_pos.column
    );
    writeln(s);
}

void token_cannot_start_expr_error(SyntaxReporter* reporter, Token token) {
    auto loc = reporter.source.get_loc(token.span.start);
    const s = "Error: The token %s(%s) cannot start an expression at:\n%s (line: %s, column: %s)".format(
        token.type,
        reporter.source.get_text(token.span),
        reporter.source.name,
        loc.line,
        loc.column
    );
    writeln(s);
}

void token_is_not_an_operator_error(SyntaxReporter* reporter, Token token) {
    auto loc = reporter.source.get_loc(token.span.start);
    const s = "Error: The token %s(%s) is not an operator:\n%s (line: %s, column: %s)".format(
        token.type,
        reporter.source.get_text(token.span),
        reporter.source.name,
        loc.line,
        loc.column
    );
    writeln(s);
}

void token_type_mismatch_error(SyntaxReporter* reporter, Token token, Token.Type expected) {
    auto loc = reporter.source.get_loc(token.span.start);
    const s = "Error: The token %s(%s) at (line: %s, column: %s) was expected to be a %s".format(
        token.type,
        reporter.source.get_text(token.span),
        loc.line,
        loc.column,
        expected,
    );
    writeln(s);
}