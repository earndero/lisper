public class Lexer {
    private final CharScanner scanner;
    private Token token;

    Lexer(CharScanner scanner) {
        this.scanner = scanner;
    }

    Token la() {
        if (token==null)
            getNext();
        return token;
    }

    void match() {
        token = null;
    }

    static boolean isDigitOrDot(char c) {
        return Character.isDigit(c) || c=='.';
    }

    static boolean is_punct(char ch) {
        return (ch>=33 && ch<=47)||(ch>=58 && ch<=64)||(ch>=91 && ch<=96)
                ||(ch>=123 && ch<=126);
    }

    // Is this character a valid lisp symbol character
    static boolean is_symbol(char ch) {
        return (Character.isAlphabetic(ch) || Character.isDigit(ch) || is_punct(ch))
                && ch != '(' && ch != ')' && ch != '"' && ch != '\''
                && ch != '#';
    }

    private Token parseNumber() {
        scanner.setAnchor();
        while(isDigitOrDot(scanner.peek()))
            if (!scanner.nextChar()) break;
        String s = scanner.getAnchor();
        if (s.indexOf('.')>=0)
            token = new Token(TT.Symbol, TST.Float, s);
        else
            token = new Token(TT.Symbol, TST.Int, s);
        return token;
    }

    /*
    * -1: not number
    * 0: integer, digits, can be start with + or -, no dots, not "s" or "d" or "e"
    * 1: float: single dot
    * 2: float no dot, exponent e
    * 3: dot and exponent e
    * 4: float no dot, exponent s
    * 5: dot and exponent s
    * 6: float no dot, exponent d
    * 7: dot and exponent d
    * */
    private int checkNumber(String s) {
        if (s.isEmpty()) return -1;
        int pos = 0;
        if (s.charAt(0)=='+' || s.charAt(0)=='-')
            pos++;
        if (s.length()<pos+1) return -1;
        int numDots = 0;
        int numDigits = 0;
        while (pos<s.length()) {
            char c = s.charAt(pos);
            if (c=='.') {
                if (numDots>0) return -1;
                numDots++;
            } else {
                if (Character.isDigit(c))
                    numDigits++;
                else
                    break;
            }
            pos++;
        }
        if (numDigits==0) return -1;
        if (pos>=s.length()) {
            if (numDots==0) return 0;
            else return 1;
        }
        int numtype;
        char c = Character.toLowerCase(s.charAt(pos));
        if (c=='e') numtype=2;
        else if (c=='s') numtype=4;
        else if (c=='d') numtype=6;
        else return -1;
        if (numDots>0) numtype++;
        pos++;
        if (pos>=s.length()) return -1;
        c = s.charAt(pos);
        if (s.charAt(pos)=='+' || s.charAt(pos)=='-')
            pos++;
        if (s.length()<pos+1) return -1;
        while (pos<s.length()) {
            if (!Character.isDigit(s.charAt(pos))) return -1;
            pos++;
        }
        return numtype;
    }

    boolean eof() {
        scanner.skip_nocode();
        return scanner.eof();
    }

    Token readString() {
        Token tok;
        scanner.skip_whitespace();//for start_value
        scanner.nextChar();
        scanner.setAnchor();
        while ((scanner.peek()!='"'))
            if (!scanner.nextChar()) break;
        if (scanner.peek()=='"')
            tok = new Token(TT.String, TST.NoSymbol, scanner.getAnchor());
        else
            tok = new Token(TT.Error, TST.NoSymbol, scanner.getAnchor());
        scanner.nextChar();
        return tok;
    }

    boolean getNext() {
        scanner.skip_nocode();
        if (scanner.eof()) {
            return false;
        }
        char c = scanner.peek();
        if (c=='"') {
            token = readString();
        }
        else if (c=='(') {
            token = new Token(TT.LParen,  TST.NoSymbol, "(");
            scanner.nextChar();
        }
        else if (c==')') {
            token = new Token(TT.RParen,  TST.NoSymbol, ")");
            scanner.nextChar();
        }
        else if (c=='#') {
            scanner.nextChar();
            char c1 = scanner.peek();
            if (c1=='(') {
                token = new Token(TT.SharpLParent,  TST.NoSymbol, "#");
                scanner.nextChar();
            } else if (c1=='\'') {
                token = new Token(TT.SharpQuot,  TST.NoSymbol, "#");
                scanner.nextChar();
            } else token = new Token(TT.Error,  TST.NoSymbol, "#");
        }
        else if (c=='\'') {
            token = new Token(TT.Quote, TST.NoSymbol, "'");
            scanner.nextChar();
        } else if (is_symbol(c)) {
            scanner.setAnchor();
            while ((is_symbol(scanner.peek())))
                if (!scanner.nextChar()) break;
            String s = scanner.getAnchor();
            int n = checkNumber(s);
            if (n<0)
                token = new Token(TT.Symbol, TST.Ident, s);
            else if (n==0)
                token = new Token(TT.Symbol, TST.Int, s);
            else {
                token = new Token(TT.Symbol, TST.Float, s);
                token.numType= n;
            }
        }
        return true;
    }

    public void readToEol() {
        while (scanner.nextCharEol());
    }
}
