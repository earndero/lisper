import java.util.List;

public class Runner {
    // Parse an entire program and get its list of expressions.
    static List<Value> parse(String s) { //todo move to other place? todo runner
        CharScanner scanner = new CharScanner(s);
        Lexer lexer = new Lexer(scanner);
        Parser parser = new Parser(lexer);
        return parser.values();
    }

    // Execute code in an environment
    static Value run(String code, Environment env) { //todo here? runner
        // Parse the code
        List<Value> parsed = parse(code);
        // Iterate over the expressions and evaluate them
        // in this environment.
        for (int i=0; i<parsed.size()-1; i++)
            parsed.get(i).eval(env);
        // Return the result of the last expression.
        return parsed.get(parsed.size()-1).eval(env);
    }
}
