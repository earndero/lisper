public class Lexer {
    private final CharScanner scanner;
    private Token token;

    Lexer(CharScanner scanner) {
        this.scanner = scanner;
    }

    Token peek() {
        return token;
    }

    static boolean isDigitOrDot(char c) {
        return Character.isDigit(c) || c=='.';
    }

    boolean next() {
        scanner.skip_nocode();
        if (scanner.eof()) {
            //token = new Token(TokenType.Eof, "");
            return false;
        }
        char c = scanner.peek();
        if (Character.isDigit(c)) {
            scanner.setAnchor();
            while(isDigitOrDot(scanner.peek()))
                if (!scanner.nextChar()) break;
            String s = scanner.getAnchor();
            if (s.indexOf('.')>=0)
                token = new Token(TokenType.Float, s);
            else
                token = new Token(TokenType.Int, s);
        }
        else if (Character.isAlphabetic(c)) {
            scanner.setAnchor();
            while ((Character.isAlphabetic(scanner.peek())))
                if (!scanner.nextChar()) break;
            String s = scanner.getAnchor();
            token = new Token(TokenType.Ident, s);
        }
        else if (c=='"') {
            scanner.nextChar();
            scanner.setAnchor();
            while ((scanner.peek()!='"'))
                if (!scanner.nextChar()) break;
            if (scanner.peek()=='"')
                 token = new Token(TokenType.String, scanner.getAnchor());
             else
                 token = new Token(TokenType.Error, scanner.getAnchor());
            scanner.nextChar();
        }
        else if (c=='@') {
            token = new Token(TokenType.At, "@");
            scanner.nextChar();
        }
        else if (c=='#') {
            token = new Token(TokenType.Sharp, "#");
            scanner.nextChar();
        }
        else if (c=='=') {
            String s="";
            s+=c;
            token = new Token(TokenType.OpCompare, s);
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
            token = new Token(TokenType.OpCompare, s);
        }
        else if (c=='+' || c=='-' || c=='*' || c=='/' || c=='%') {
            String s="";
            s+=c;
            token = new Token(TokenType.OpArith, s);
            scanner.nextChar();
        } else {
            String s="";
            s+=c;
            token = new Token(TokenType.Error, s);
            scanner.nextChar();
        }
        //scanner.nextChar();
        return true;
    }
}
