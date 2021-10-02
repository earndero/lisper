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

    StartValue start_value() {
        Token tok = lexer.la();
        StartValue sv = null;
        if (tok.type==TT.LParen) {
            List<Value> l = list();
            sv = new StartValue(l);
        } else
            throw new Error(null, new Environment(), Error.EXPRESSION_NO_LIST);
        tok = lexer.la();
        if (tok!=null)
        if (tok.value.equals("=>") || tok.value.equals("=e>")) {
            lexer.readToEol();
            lexer.getNext();
        }
        return sv;
    }

    Value value() {
        Token tok = lexer.la();
        if (tok.type==TT.LParen) {
            List<Value> l = list();
            return new Value(l);
        }
        else {
            lexer.match();
            switch(tok.type) {
                case String: return Value.string(tok.value);
                case Symbol:
                    switch(tok.subtype){
                        case Ident:  return Value.atom(tok.value);
                        case Int: return new Value(new BigRational(tok.value));
                        case Float: return new Value(Double.parseDouble(tok.value));
                    }
                case Quote: return Value.quote(value());
                default:
                    throw new Error(null, new Environment(), Error.MALFORMED_PROGRAM);
            }
        }
    }

    List<Value> values() {
        List<Value> result = new ArrayList<>();
        while (!lexer.eof()) {
            StartValue v = start_value();
            result.add(v);
        }
        return result;
    }
}
