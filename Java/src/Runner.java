import java.util.ArrayList;
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

    static Value print_eval(StartValue arg, Environment env) {
        Value value = arg.eval(env);
        if (arg.testResult==null)
            System.out.println(value.display());
        else if (value.display().equals((arg.testResult)))
            System.out.println("OK");
        else
            System.out.println("diff: "+value.display()+" <-> "+arg.testResult);
        return value;
    }

    // Execute code in an environment
    static Value run(String code, Environment env) {
        // Parse the code
        List<StartValue> parsed = parse_start(code);
        // Iterate over the expressions and evaluate them
        // in this environment.
        for (int i=0; i<parsed.size()-1; i++)
            print_eval(parsed.get(i), env);
        // Return the result of the last expression.
        return print_eval(parsed.get(parsed.size()-1), env);
    }

    static void run_start(String[] argv, String code) {
        List<Value> args = new ArrayList<>();
        for (int i=0; i<argv.length; i++)
            args.add(Value.string(argv[i]));
        int argc = argv.length;
        Environment env = new Environment();
        env.set("cmd-args", new Value(args));
        run(code, env);
    }
}
