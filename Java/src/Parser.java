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
        Token tok = lexer.la();
        if (tok.type==TT.LParen) {
            List<Value> l = list();
            return new Value(l);
        }
        else {
            lexer.match();
            switch(tok.type) {
                case At: return new Value();
                case String: return Value.string(tok.value);
                case Atom:case OpCompare:case OpArith:  return Value.atom(tok.value);
                case Int: return new Value(Integer.parseInt(tok.value));
                case Float: return new Value(Double.parseDouble(tok.value));
                case Quote: return Value.quote(value());
                default:
                    throw new Error(null, new Environment(), Error.MALFORMED_PROGRAM);
            }
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
