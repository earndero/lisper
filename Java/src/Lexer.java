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

    static boolean isAlphaOrUnder(char c) {
        return Character.isAlphabetic(c) || c=='_';
    }

    static boolean isAlphaOrUnderHyp(char c) {
        return isAlphaOrUnder(c) || c=='-';
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

    boolean eof() {
        scanner.skip_nocode();
        return scanner.eof();
    }

    boolean getNext() {
        scanner.skip_nocode();
        if (scanner.eof()) {
            //token = new Token(TokenType.Eof, "");
            return false;
        }
        char c = scanner.peek();
        if (Character.isDigit(c)) {
            token = parseNumber();
        }
        else if (isAlphaOrUnder(c)) {
            scanner.setAnchor();
            while ((isAlphaOrUnderHyp(scanner.peek())))
                if (!scanner.nextChar()) break;
            String s = scanner.getAnchor();
            token = new Token(TT.Symbol, TST.Ident, s);
        }
        else if (c=='"') {
            scanner.nextChar();
            scanner.setAnchor();
            while ((scanner.peek()!='"'))
                if (!scanner.nextChar()) break;
            if (scanner.peek()=='"')
                 token = new Token(TT.String, TST.NoSymbol, scanner.getAnchor());
             else
                 token = new Token(TT.Error, TST.NoSymbol, scanner.getAnchor());
            scanner.nextChar();
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
        }
        else if (c=='=') {
            String s="";
            s+=c;
            token = new Token(TT.Symbol, TST.Ident, s);
            scanner.nextChar();
        }
        else if (c=='<' || c=='>') {
            String s="";
            s+=c;
            scanner.nextChar();
            char c1 = scanner.peek();
            if (c1=='=') {
                s+=c1;
                scanner.nextChar();
            }
            token = new Token(TT.Symbol, TST.Ident, s);
        }
        else if (c=='!') {
            String s="";
            s+=c;
            scanner.nextChar();
            char c1 = scanner.peek();
            if (c1=='=') {
                s+=c1;
                scanner.nextChar();
                token = new Token(TT.Symbol, TST.Ident, s);
            } else token = new Token(TT.Error,TST.NoSymbol, s);
        }
        else if (c=='+' || c=='-' || c=='*' || c=='/' || c=='%') {
            String s="";
            s+=c;
            scanner.nextChar();
            if (c=='-' && Character.isDigit(scanner.peek()))
                token = parseNumber();
            else
                token = new Token(TT.Symbol, TST.Ident, s);
        } else {
            String s="";
            s+=c;
            token = new Token(TT.Error,TST.NoSymbol, s);
            scanner.nextChar();
        }
        //scanner.nextChar();
        return true;
    }

}
