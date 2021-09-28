enum TokenType {Eof, Error, Ident, Int, Float, String, OpCompare,OpArith, At, Sharp}

public class Token {
    TokenType type;
    String value;
    Token(TokenType type, String value) {
        this.type = type;
        this.value= value;;
    }
}
