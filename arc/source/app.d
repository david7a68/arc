import std.stdio;
import arc.syntax.lexer;

void main() {
	const str = "[[]]],19281_2918 (_aiw19)_";
    const types = [
        Token.Lbracket, Token.Lbracket,Token.Rbracket, Token.Rbracket, Token.Rbracket, Token.Comma,
        Token.Integer, Token.Lparen, Token.Name, Token.Rparen, Token.Name
    ];

    Lexer lexer;
    lexer.reset(str);
    foreach (i; 0 .. types.length) {
        lexer.lex();

		const token = lexer.current;
        assert(token.type == types[i]);
		writeln(token.start[0..token.span]);
    }
}
