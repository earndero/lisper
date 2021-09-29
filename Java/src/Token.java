enum TT {Eof, Error, LParen, RParen, Atom, Int, Float, String, Quote, OpCompare,OpArith, At, Sharp}

public class Token {
    TT type;
    String value;
    Token(TT type, String value) {
        this.type = type;
        this.value= value;;
    }
}
