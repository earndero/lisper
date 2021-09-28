import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;

// C++ namespace to Java class with static methods
// This namespace contains all the definitions of builtin functions
public class builtin {

    static Random rand = new Random();

    static String read_file_contents(String filename) {
        List<String> lines = null;
        try {
            lines = Files.readAllLines(Paths.get(filename));
        } catch (IOException e) {
            e.printStackTrace();
        }
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i< lines.size();i++) {
            String line = lines.get(i);
            if (i>0)
                sb.append('\n');
            sb.append(line);
        }
        return sb.toString();
    }

    // This function is NOT a builtin function, but it is used
    // by almost all of them.
    //
    // Special forms are just builtin functions that don't evaluate
    // their arguments. To make a regular builtin that evaluates its
    // arguments, we just call this function in our builtin definition.
    static void eval_args(List<Value> args, Environment env) {
        for (int i = 0; i < args.size(); i++) {
            Value arg = args.get(i);
            Value evaluated = arg.eval(env);
            args.set(i, evaluated);
        }
    }

    // Create a lambda function (SPECIAL FORM)
    static Builtin lambda = new Builtin() {
        @Override
        public Value apply(Param p) {
            if (p.args.size() < 2)
                throw new Error(new Value("lambda", lambda), p.env, Error.TOO_FEW_ARGS);
            if (p.args.get(0).get_type_name() != Value.LIST_TYPE)
                throw new Error(new Value("lambda", lambda), p.env, Error.INVALID_LAMBDA);
            return new Value(p.args.get(0).as_list(), p.args.get(1), p.env);
        }
    };

    // if-else (SPECIAL FORM)
    static Builtin if_then_else = new Builtin() {
        @Override
        public Value apply(Param p) {
            if (p.args.size() != 3)
                throw new Error(new Value("if", if_then_else), p.env, p.args.size() > 3 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            if (p.args.get(0).eval(p.env).as_bool())
                return p.args.get(1).eval(p.env);
            else return p.args.get(2).eval(p.env);
        }
    };

    // Define a variable with a value (SPECIAL FORM)
    static Builtin define = new Builtin() {
        @Override
        public Value apply(Param p) {
            if (p.args.size() != 2)
                throw new Error(new Value("define", define), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            Value result = p.args.get(1).eval(p.env);
            p.env.set(p.args.get(0).display(), result);
            return result;
        }
    };

    // Define a function with parameters and a result expression (SPECIAL FORM)
    static Builtin defun = new Builtin() {
        @Override
        public Value apply(Param p) {
            if (p.args.size() != 3)
                throw new Error(new Value("defun", defun), p.env, p.args.size() > 3 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            if (p.args.get(1).get_type_name() != Value.LIST_TYPE)
                throw new Error(new Value("defun", defun), p.env, Error.INVALID_LAMBDA);

            Value f = new Value(p.args.get(1).as_list(), p.args.get(2), p.env);
            p.env.set(p.args.get(0).display(), f);
            return f;
        }
    };

    // Loop over a list of expressions with a condition (SPECIAL FORM)
    static Builtin while_loop = new Builtin() {
        @Override
        public Value apply(Param p) {
            Value acc= new Value();
            while (p.args.get(0).eval(p.env).as_bool()) {
                for (int i = 1; i < p.args.size() - 1; i++)
                    p.args.get(i).eval(p.env);
                acc = p.args.get(p.args.size() - 1).eval(p.env);
            }
            return acc;
        }
    };

    // Iterate through a list of values in a list (SPECIAL FORM)
    static Builtin for_loop = new Builtin() {
        @Override
        public Value apply(Param p) {
            Value acc = new Value();
            List<Value> list = p.args.get(1).eval(p.env).as_list();

            for (int i = 0; i < list.size(); i++) {
                p.env.set(p.args.get(0).as_atom(), list.get(i));

                for (int j = 1; j < p.args.size() - 1; j++)
                    p.args.get(j).eval(p.env);
                acc = p.args.get(p.args.size() - 1).eval(p.env);
            }

            return acc;
        }
    };

    // Evaluate a block of expressions in the current environment (SPECIAL FORM)
    static Builtin do_block = new Builtin() {
        @Override
        public Value apply(Param p) {
            Value acc = new Value();
            for (int i = 0; i < p.args.size(); i++)
                acc = p.args.get(i).eval(p.env);
            return acc;
        }
    };

    // Evaluate a block of expressions in a new environment (SPECIAL FORM)
    static Builtin scope = new Builtin() {
        @Override
        public Value apply(Param p) {
            Environment e = p.env;
            Value acc=  new Value();
            for (int i = 0; i < p.args.size(); i++)
                acc = p.args.get(i).eval(e);
            return acc;
        }
    };

    static Builtin quote = new Builtin() {
        @Override
        public Value apply(Param p) {
            List<Value> v = new ArrayList<>();
            for (int i = 0; i < p.args.size(); i++)
                v.add(p.args.get(i));
            return new Value(v);
        }
    };

    //#ifdef USE_STD
    // Exit the program with an integer code
    static Builtin exit = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            System.exit(p.args.size() < 1 ? 0 : p.args.get(0).cast_to_int().as_int());
            return new Value();
        }
    };

    // Print several values and return the last one
    static Builtin print = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() < 1)
                throw new Error(new Value("print", print), p.env, Error.TOO_FEW_ARGS);

            Value acc = new Value();
            for (int i = 0; i < p.args.size(); i++) {
                acc = p.args.get(i);
                System.out.print(acc.display());
                if (i < p.args.size() - 1)
                    System.out.print(" ");
            }
            System.out.println();
            return acc;
        }
    };


    // Get user input with an optional prompt
    static Builtin input = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() > 1)
                throw new Error(new Value("input", input), p.env, Error.TOO_MANY_ARGS);

            if (!p.args.isEmpty())
                p.args.get(0).print();

            /*Scanner cin = new Scanner(System.in);
            String s = cin.nextLine();
            cin.close();*/
            String s = "4";
            return Value.string(s);
        }
    };

    // Get a random number between two numbers inclusively
    static Builtin random = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("random", random), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            int low = p.args.get(0).as_int(), high = p.args.get(1).as_int();
            return new Value(rand.nextInt(high - low + 1)  + low);
        }
    };

    // Get the contents of a file
    static Builtin read_file = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("read-file", read_file), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            return Value.string(read_file_contents(p.args.get(0).as_string()));
        }
    };

    // Write a string to a file
    static Builtin write_file = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("write-file", write_file), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            try {
                // The first argument is the file name
                PrintWriter writer = new PrintWriter(new File(p.args.get(0).as_string()));
                // The second argument is the contents of the file to write
                writer.write(p.args.get(1).as_string());
                return new Value(1);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                return new Value(0);
            }
        }
    };

    static Value run(String code, Environment env) {
        // Parse the code
        List<Value> parsed = Value.parse(code);
        // Iterate over the expressions and evaluate them
        // in this environment.
        for (int i=0; i<parsed.size()-1; i++)
            parsed.get(i).eval(env);

        // Return the result of the last expression.
        return parsed.get(parsed.size()-1).eval(env);
    }

    // Read a file and execute its code
    static Builtin include = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Import is technically not a special form, it's more of a macro.
            // We can evaluate our arguments.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("include", include), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            Environment e = new Environment();
            Value result = null;
            result = run(read_file_contents(p.args.get(0).as_string()), e);
            p.env.combine(e);
            return result;
        }
    };

//#endif

    // Evaluate a value as code
    static Builtin eval = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("eval", eval), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            else return p.args.get(0).eval(p.env);
        }
    };

    // Create a list of values
    static Builtin list = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            return new Value(p.args);
        }
    };

    // Sum multiple values
    static Builtin sum = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() < 2)
                throw new Error(new Value("+", sum), p.env, Error.TOO_FEW_ARGS);

            Value acc = p.args.get(0);
            for (int i = 1; i < p.args.size(); i++)
                acc = acc.add(p.args.get(i));
            return acc;
        }
    };

    // Subtract two values
    static Builtin subtract = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("-", subtract), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            return p.args.get(0).sub(p.args.get(1));
        }
    };

    // Multiply several values
    static Builtin product = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() < 2)
                throw new Error(new Value("*", product), p.env, Error.TOO_FEW_ARGS);

            Value acc = p.args.get(0);
            for (int i = 1; i < p.args.size(); i++)
                acc = acc.mul(p.args.get(i));
            return acc;
        }
    };

    // Divide two values
    static Builtin divide = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("/", divide), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            return p.args.get(0).div(p.args.get(1));
        }
    };

    // Get the remainder of values
    static Builtin remainder = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("%", remainder), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            return p.args.get(0).mod(p.args.get(1));
        }
    };

    // Are two values equal?
    static Builtin eq = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("=", eq), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            int n = p.args.get(0).eq(p.args.get(1)) ?1:0;
            return new Value(n);
        }
    };

    // Are two values not equal?
    static Builtin neq = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("!=", neq), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            int n = p.args.get(0).neq(p.args.get(1))?1:0;
            return new Value(n);
        }
    };

    // Is one number greater than another?
    static Builtin greater = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value(">", greater), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            int n = p.args.get(0).gt(p.args.get(1))?1:0;
            return new Value(n);
        }
    };

    // Is one number less than another?
    static Builtin less = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("<", less), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            int n = p.args.get(0).lt(p.args.get(1))?1:0;
            return new Value(n);
        }
    };

    // Is one number greater than or equal to another?
    static Builtin greater_eq = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value(">=", greater_eq), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            int n = p.args.get(0).gte(p.args.get(1))?1:0;
            return new Value(n);
        }
    };

    // Is one number less than or equal to another?
    static Builtin less_eq = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("<=", less_eq), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            int n = p.args.get(0).lte(p.args.get(1))?1:0;
            return new Value(n);
        }
    };

    // Get the type name of a value
    static Builtin get_type_name = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);
            if (p.args.size() != 1)
                throw new Error(new Value("type", get_type_name), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            return Value.string(p.args.get(0).get_type_name());
        }
    };

    // Cast an item to a float
    static Builtin cast_to_float = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value(Value.FLOAT_TYPE, cast_to_float), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            return p.args.get(0).cast_to_float();
        }
    };

    // Cast an item to an int
    static Builtin cast_to_int = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value(Value.INT_TYPE, cast_to_int), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            return p.args.get(0).cast_to_int();
        }
    };

    // Index a list
    static Builtin index = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("index", index), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            List<Value> list = p.args.get(0).as_list();
            int i = p.args.get(1).as_int();
            if (list.isEmpty() || i >= list.size())
                throw new Error(new Value(list), p.env, Error.INDEX_OUT_OF_RANGE);

            return list.get(i);
        }
    };

    // Insert a value into a list
    static Builtin insert = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 3)
                throw new Error(new Value("insert", insert), p.env, p.args.size() > 3 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            List<Value> list = p.args.get(0).as_list();
            int i = p.args.get(1).as_int();
            if (i > list.size())
                throw new Error(new Value(list), p.env, Error.INDEX_OUT_OF_RANGE);

            list.add( p.args.get(1).as_int(), p.args.get(2) );
            return new Value(list);
        }
    };

    // Remove a value at an index from a list
    static Builtin remove = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 2)
                throw new Error(new Value("remove", remove), p.env, p.args.size() > 2 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            List<Value> list = p.args.get(0).as_list();
            int i = p.args.get(1).as_int();
            if (list.isEmpty() || i >= list.size())
                throw new Error(new Value(list), p.env, Error.INDEX_OUT_OF_RANGE);

            list.remove(i);
            return new Value(list);
        }
    };

    // Get the length of a list
    static Builtin len = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("len", len), p.env, p.args.size() > 1 ?
                        Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS
                );

            return new Value(p.args.get(0).as_list().size());
        }
    };

    // Add an item to the end of a list
    static Builtin push = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() == 0)
                throw new Error(new Value("push", push), p.env, Error.TOO_FEW_ARGS);
            for (int i = 1; i < p.args.size(); i++)
                p.args.get(0).push(p.args.get(i));
            return p.args.get(0);
        }
    };

    static Builtin pop = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("pop", pop), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            return p.args.get(0).pop();
        }
    };

    static Builtin head = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("head", head), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            List<Value> list = p.args.get(0).as_list();
            if (list.isEmpty())
                throw new Error(new Value("head", head), p.env, Error.INDEX_OUT_OF_RANGE);

            return list.get(0);
        }
    };

    static Builtin tail = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("tail", tail), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            List<Value> result = new ArrayList<>();
            List<Value> list = p.args.get(0).as_list();

            for (int i = 1; i < list.size(); i++)
                result.add(list.get(i));

            return new Value(result);
        }
    };

    static Builtin parse = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("parse", parse), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);
            if (!p.args.get(0).get_type_name().equals(Value.STRING_TYPE))
                throw new Error(p.args.get(0), p.env, Error.INVALID_ARGUMENT);
            List<Value> parsed = Value.parse(p.args.get(0).as_string());

            // if (parsed.size() == 1)
            //     return parsed.get(0);
            // else return Value(parsed);
            return new Value(parsed);
        }
    };

    static Builtin replace = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 3)
                throw new Error(new Value("replace", replace), p.env, p.args.size() > 3 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            String src = p.args.get(0).as_string();
            src = src.replaceAll(p.args.get(1).as_string(), p.args.get(2).as_string());
            return Value.string(src);
        }
    };

    static Builtin display = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("display", display), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            return Value.string(p.args.get(0).display());
        }
    };

    static Builtin debug = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            if (p.args.size() != 1)
                throw new Error(new Value("debug", debug), p.env, p.args.size() > 1 ? Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS);

            return Value.string(p.args.get(0).debug());
        }
    };

    static Builtin map_list = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            List<Value> result = new ArrayList<>();
            List<Value> l = p.args.get(1).as_list();
            List<Value> tmp = new ArrayList<>();
            for (int i = 0; i < l.size(); i++) {
                tmp.add(l.get(i));
                result.add(p.args.get(0).apply(tmp, p.env));
                tmp.clear();
            }
            return new Value(result);
        }
    };

    static Builtin filter_list = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            List<Value> result = new ArrayList<>();
            List<Value> l = p.args.get(1).as_list();
            List<Value> tmp = new ArrayList<>();
            for (int i = 0; i < l.size(); i++) {
                tmp.add(l.get(i));
                if (p.args.get(0).apply(tmp, p.env).as_bool())
                    result.add(l.get(i));
                tmp.clear();
            }
            return new Value(result);
        }
    };

    static Builtin reduce_list = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            List<Value> l = p.args.get(2).as_list();
            List<Value> tmp = new ArrayList<>();
            Value acc = p.args.get(1);
            for (int i = 0; i < l.size(); i++) {
                tmp.add(acc);
                tmp.add(l.get(i));
                acc = p.args.get(0).apply(tmp, p.env);
                tmp.clear();
            }
            return acc;
        }
    };

    static Builtin range = new Builtin() {
        @Override
        public Value apply(Param p) {
            // Is not a special form, so we can evaluate our p.args.
            eval_args(p.args, p.env);

            List<Value> result = new ArrayList<>();
            Value low = p.args.get(0), high = p.args.get(1);
            if (!low.get_type_name().equals(Value.INT_TYPE) && !low.get_type_name().equals(Value.FLOAT_TYPE))
                throw new Error(low, p.env, Error.MISMATCHED_TYPES);
            if (!high.get_type_name().equals(Value.INT_TYPE) && !high.get_type_name().equals(Value.FLOAT_TYPE))
                throw new Error(high, p.env, Error.MISMATCHED_TYPES);

            if (low.gte(high)) return new Value(result);

            while (low.lt(high)) {
                result.add(low);
                low = low.add(new Value(1));
            }
            return new Value(result);
        }
    };
}

