import java.util.ArrayList;
import java.util.List;

public class Parser {
    Lexer lexer;
    public Parser(Lexer lexer) {
        this.lexer = lexer;
    }
    void atom(){
        System.out.println(lexer.la().value);
        lexer.match();
    }

    List<Value>  list() {
        List<Value> result = new ArrayList<>();
        lexer.match();
        while (!lexer.eof() && lexer.la().type!=TT.RParen) {
            Value v = value();
            result.add(v);
        }
        lexer.match();
        return result;
    }

    Value value() {
        if (lexer.la().type==TT.LParen) {
            List<Value> l = list();
            return new Value(l);
        }
        else {
            atom();
            return null;//todo 
        }
    }

    List<Value> values() {
        List<Value> result = new ArrayList<>();
        while (!lexer.eof()) {
            Value v = value();
            result.add(v);
        }
        return result;
    }
}
