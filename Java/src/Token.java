//token type
enum TT {Eof, Error, LParen, RParen, String, Quote, Symbol, SharpLParent,SharpQuot};

//token subtype for symbol
enum TST {NoSymbol, Int,Float,Ident};

public class Token {
    TT type;
    TST subtype;
    String value;
    Token(TT type, TST subtype, String value) {
        this.type = type;
        this.subtype = subtype;
        this.value= value;;
    }
}
