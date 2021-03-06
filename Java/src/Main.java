import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
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
                tmp = Runner.run(input, env);
                System.out.println(" => " + tmp.debug() );
                code += input + "\n";
            }
        }
        cin.close();
//#endif
    }


    // Execute code in an environment
    public static void main(String[] argv) {
        File f = new File("examples/v");
        FilenameFilter filter = new FilenameFilter() {
            @Override
            public boolean accept(File f, String name) {
                return name.endsWith(".lisp");
            }
        };
        String[] pathnames = f.list();
        Arrays.sort(pathnames);
        String[] names = {"optional_alone.lisp", "simple_params.lisp",
                "opt_params.lisp", "key_params.lisp", "optional.lisp",
                "def_values.lisp", "lambda_key.lisp" };
        //Runner.run_start(argv, builtin.read_file_contents("examples/v/optional.lisp"));
        for (String name : names) {
            System.out.println(name);
            Runner.run_start(argv, builtin.read_file_contents("examples/v/"+name));
        }

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
