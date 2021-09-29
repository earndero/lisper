public class Parser {
    Lexer lexer;
    public Parser(Lexer lexer) {
        this.lexer = lexer;
    }
    void atom(){
        System.out.println(lexer.la().value);
        lexer.match();
    }

    void list() {
        lexer.match();
        while (!lexer.eof() && lexer.la().type!=TT.RParen) {
            value();
        }
        lexer.match();
    }

    void value() {
        if (lexer.la().type==TT.LParen) list();
        else atom();
    }

    void values() {
        while (!lexer.eof()) {
            value();
        }
    }
}
