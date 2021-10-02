import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

// An instance of a function's scope.
public class Environment {
    // The definitions in the scope.
    private Map<String, Value> defs = new HashMap<>();;
    private Environment parent_scope;

    public Environment clone() {
        Environment cloned = new Environment();
        cloned.parent_scope = parent_scope;
        for (Map.Entry<String, Value> entry : defs.entrySet()) {
            String key = entry.getKey();
            Value value = entry.getValue();
            cloned.defs.put(key, value.clone());
        }
        return cloned;
    }


    public Environment() {
        parent_scope = null;
    }
    // Does this environment, or its parent environment,
    // have this atom in scope?
    // This is only used to determine which atoms to capture when
    // creating a lambda function.

    // Does this environment, or its parent environment, have a variable?
    boolean has(String name) {
        // Find the value in the map
        if (defs.containsKey(name))
            // If it was found
            return true;
        else if (parent_scope != null)
            // If it was not found in the current environment,
            // try to find it in the parent environment
            return parent_scope.has(name);
        else return false;
    }

    // Get the value associated with this name in this scope
    Value get(String name) {
        name = name.toLowerCase(Locale.ROOT);
        // Meta operations
        if (name.equals("eval"))  return new Value("eval",  builtin.eval);
        if (name.equals("type"))  return new Value("type",  builtin.get_type_name);
        if (name.equals("parse")) return new Value("parse", builtin.parse);

        // Special forms
        if (name.equals("do"))     return new Value("do",     builtin.do_block);
        if (name.equals("if"))     return new Value("if",     builtin.if_then_else);
        if (name.equals("cond"))   return new Value("cond",     builtin.cond);
        if (name.equals("for"))    return new Value("for",    builtin.for_loop);
        if (name.equals("while"))  return new Value("while",  builtin.while_loop);
        if (name.equals("scope"))  return new Value("scope",  builtin.scope);
        if (name.equals("quote"))  return new Value("quote",  builtin.quote);
        if (name.equals("defun"))  return new Value("defun",  builtin.defun);
        if (name.equals("define")) return new Value("define", builtin.define);
        if (name.equals("setq")) return new Value("setq", builtin.setq);
        if (name.equals("lambda")) return new Value("lambda", builtin.lambda);

        // Comparison operations
        if (name.equals("="))  return new Value("=",  builtin.eq);
        if (name.equals("!=")) return new Value("!=", builtin.neq);
        if (name.equals(">"))  return new Value(">",  builtin.greater);
        if (name.equals("<"))  return new Value("<",  builtin.less);
        if (name.equals(">=")) return new Value(">=", builtin.greater_eq);
        if (name.equals("<=")) return new Value("<=", builtin.less_eq);

        // Functions returns boolean
        if (name.equals("evenp")) return new Value("evenp", builtin.evenp);
        if (name.equals("oddp")) return new Value("oddp", builtin.oddp);

        // Arithmetic operations
        if (name.equals("+")) return new Value("+", builtin.sum);
        if (name.equals("-")) return new Value("-", builtin.subtract);
        if (name.equals("*")) return new Value("*", builtin.product);
        if (name.equals("/")) return new Value("/", builtin.divide);
        if (name.equals("%")) return new Value("%", builtin.remainder);

        // List operations
        if (name.equals("list"))   return new Value("list",   builtin.list);
        if (name.equals("insert")) return new Value("insert", builtin.insert);
        if (name.equals("index"))  return new Value("index",  builtin.index);
        if (name.equals("remove")) return new Value("remove", builtin.remove);

        if (name.equals("len"))    return new Value("len",   builtin.len);

        if (name.equals("push"))   return new Value("push",  builtin.push);
        if (name.equals("pop"))    return new Value("pop",   builtin.pop);
        if (name.equals("head"))   return new Value("head",  builtin.head);
        if (name.equals("tail"))   return new Value("tail",  builtin.tail);
        if (name.equals("first"))  return new Value("first", builtin.head);
        if (name.equals("last"))   return new Value("last",  builtin.pop);
        if (name.equals("range"))  return new Value("range", builtin.range);

        // Functional operations
        if (name.equals("map"))    return new Value("map",    builtin.map_list);
        if (name.equals("filter")) return new Value("filter", builtin.filter_list);
        if (name.equals("reduce")) return new Value("reduce", builtin.reduce_list);

        // IO operations
//#ifdef USE_STD
        if (name.equals("exit"))       return new Value("exit",       builtin.exit);
        if (name.equals("quit"))       return new Value("quit",       builtin.exit);
        if (name.equals("print"))      return new Value("print",      builtin.print);
        if (name.equals("input"))      return new Value("input",      builtin.input);
        if (name.equals("random"))     return new Value("random",     builtin.random);
        if (name.equals("include"))    return new Value("include",    builtin.include);
        if (name.equals("read-file"))  return new Value("read-file",  builtin.read_file);
        if (name.equals("write-file")) return new Value("write-file", builtin.write_file);
//#endif

        // String operations
        if (name.equals("debug"))   return new Value("debug",   builtin.debug);
        if (name.equals("replace")) return new Value("replace", builtin.replace);
        if (name.equals("display")) return new Value("display", builtin.display);

        // Casting operations
        if (name.equals("int"))   return new Value("int",   builtin.cast_to_int);
        if (name.equals("float")) return new Value("float", builtin.cast_to_float);

        // Constants
        if (name.equals("endl")) return Value.string("\n");

        Value value = defs.get(name.toUpperCase(Locale.ROOT)); //todo while insetad of call get
        if (value!=null) return value;
        else if (parent_scope != null) {
            value = parent_scope.defs.get(name.toUpperCase(Locale.ROOT));
            if (value!=null) return value;
            else return parent_scope.get(name.toUpperCase(Locale.ROOT));
        }
        throw new Error(Value.atom(name), this, Error.ATOM_NOT_DEFINED);
    }


    void combine(Environment other) {
        // Normally, I would use the `insert` method of the `map` class,
        // but it doesn't overwrite previously declared values for keys.
        for (Map.Entry<String, Value> entry : other.defs.entrySet()) {
            String key = entry.getKey();
            Value value = entry.getValue();
            // ...
            // Iterate through the keys and assign each value.
            defs.put(key,value);
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{ ");
        for (Map.Entry<String, Value> entry : defs.entrySet()) {
            String key = entry.getKey();
            Value value = entry.getValue();
            String s =  '\'' + key + "' : " + value.debug() + ", ";
            sb.append(s);
        }
        sb.append("}");
        return sb.toString();
    }

    void print(PrintWriter os) {
        os.write(toString());
    }

    void set(String name, Value value) {
        defs.put(name, value.clone());
        if (value.keyArgOnPosition>=0) {
            Value params = value.car();
            List<Value> clonedList = params.as_list();
            for (Value v: clonedList) {
                if (v.isKeyParam)
                    defs.put(v.display(), null); //todo here will default values!
            }
            System.out.println();
        }
    }

    void set_parent_scope(Environment parent) {
        parent_scope = parent;
    }

}
