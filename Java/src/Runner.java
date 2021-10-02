import java.util.List;

public class Runner {
    // Parse an entire program and get its list of expressions.
    static List<StartValue> parse_start(String s) {
        CharScanner scanner = new CharScanner(s);
        Lexer lexer = new Lexer(scanner);
        Parser parser = new Parser(lexer);
        return parser.start_values();
    }

    static List<Value> parse(String s) {
        CharScanner scanner = new CharScanner(s);
        Lexer lexer = new Lexer(scanner);
        Parser parser = new Parser(lexer);
        return parser.values();
    }

    // Execute code in an environment
    static Value run(String code, Environment env) {
        // Parse the code
        List<StartValue> parsed = parse_start(code);
        // Iterate over the expressions and evaluate them
        // in this environment.
        for (int i=0; i<parsed.size()-1; i++) {
            StartValue arg = parsed.get(i);
            Value value = arg.eval(env);
            if (arg.testResult==null)
                System.out.println(value.display());
            else if (value.display().equals((arg.testResult)))
                System.out.println("OK");
            else
                System.out.println("diff: "+value.display()+" <-> "+arg.testResult);
        }
        // Return the result of the last expression.
        StartValue arg = parsed.get(parsed.size()-1); //todo redyntant
        Value value = arg.eval(env);
        if (arg.testResult==null)
            System.out.println(value.display());
        else if (value.display().equals((arg.testResult)))
            System.out.println("OK");
        else
            System.out.println("diff: "+value.display()+" <-> "+arg.testResult);
        return value;
    }
}
