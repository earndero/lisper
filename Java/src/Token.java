enum TT {Eof, Error, LParen, RParen, Ident, Int, Float, String, OpCompare,OpArith, At, Sharp}

public class Token {
    TT type;
    String value;
    Token(TT type, String value) {
        this.type = type;
        this.value= value;;
    }
}
