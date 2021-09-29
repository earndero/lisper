import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

    static void repl(Environment env) {
//#ifdef USE_STD
        String code="";
        String input;
        Value tmp;
        List<Value> parsed = new ArrayList<>();
        Scanner cin = new Scanner(System.in);
        while (true) {
            System.out.print(">>> ");
            input = cin.nextLine();
            if (input.equals("!quit") || input.equals("!q"))
                break;
            else if (input.equals("!env") || input.equals("!e"))
                System.out.println(env.toString());
            else if (input.equals("!export") || input.equals("!x")) {
                System.out.print("File to export to: ");
                cin = new Scanner(System.in);
                input = cin.nextLine();
                cin.close();
                try {
                    PrintWriter writer = new PrintWriter(new File(input));
                    writer.write(code);
                    writer.close();
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                }
            } else if (!input.equals("")) {
                tmp = Value.run(input, env);
                System.out.println(" => " + tmp.debug() );
                code += input + "\n";
            }
        }
        cin.close();
//#endif
    }


    // Execute code in an environment
    public static void main(String[] argv) {
        Environment env = new Environment();
        List<Value> args = new ArrayList<>();
        for (int i=0; i<argv.length; i++)
            args.add(Value.string(argv[i]));
        env.set("cmd-args", new Value(args));
        int argc = argv.length;
        //Value.run(builtin.read_file_contents("examples/defun.lisp"), env);
        Value.run(builtin.read_file_contents("examples/hello_world.lisp"), env);
        Value.run(builtin.read_file_contents("examples/odd_even.lisp"), env);
        Value.run(builtin.read_file_contents("examples/io.lisp"), env);
        Value.run(builtin.read_file_contents("examples/loops.lisp"), env);
        Value.run(builtin.read_file_contents("examples/math.lisp"), env);
        Value.run(builtin.read_file_contents("examples/list.lisp"), env);
        //#ifdef USE_STD
/*
        if (argc == 0 || (argc == 1 && argv[0].equals("-i")))
            repl(env);
        else if (argc == 2 && argv[0].equals("-c"))
            Value.run(argv[1], env);
        else if (argc == 2 && argv[0].equals("-f"))
                Value.run(builtin.read_file_contents(argv[1]), env);
        else System.out.println("invalid arguments");
*/
    //#else
     //   if (argc == 3 && String(argv[1]) == "-c")
//        run(argv[2], env);
  //  #endif
    }
}
