import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

public class Value {
    static final String STRING_TYPE = "string";
    static final String INT_TYPE = "int";
    static final String FLOAT_TYPE = "float";
    static final String UNIT_TYPE = "unit";
    static final String FUNCTION_TYPE = "function";
    static final String ATOM_TYPE = "atom";
    static final String QUOTE_TYPE = "quote";
    static final String LIST_TYPE = "list";

    enum Type {QUOTE, ATOM, INT, FLOAT, LIST,STRING,
        LAMBDA, BUILTIN, UNIT};

    private List<Value> list = new ArrayList<>();
    private Environment lambda_scope = new Environment();

    private Type type;
    private String str;
    private Object stack_data; //union int i; double f; Builtin b;

    protected Value clone() {
        switch (type) {
            case INT:
                return new Value((int)stack_data);
            case FLOAT:
                return new Value((double)stack_data);
            case STRING:
                return string(str);
            case LIST:
                new Value(list); //todo need clone
            case BUILTIN:
                return new Value(str, (Builtin)stack_data);
            case UNIT:
                return new Value();
            case QUOTE:
                return quote(list.get(0));
            case ATOM:
                return atom(str);
            case LAMBDA:
                return new Value(list.get(0).as_list(), list.get(1), null);//todo env==null?
            default:return new Value();
        }
    }

    // Constructs a unit value
    Value() {
        type = Type.UNIT;
    }

    // Constructs an integer
    Value(int i) {
        type = Type.INT;
        stack_data = i;
    }
    // Constructs a floating point value
    Value(double f) {
        type = Type.FLOAT;
        stack_data = f;
    }
    // Constructs a list
    Value(List<Value> list) {
        type = Type.LIST;
        this.list = list;
    }
    // Construct a quoted value
    static Value quote(Value quoted) {
        Value result = new Value();
        result.type = Type.QUOTE;

        // The first position in the list is
        // used to store the quoted expression.
        result.list.add(quoted);
        return result;
    }

    // Construct an atom
    static Value atom(String s) {
        Value result = new Value();
        result.type = Type.ATOM;

        // We use the `str` member to store the atom.
        result.str = s;
        return result;
    }

    // Construct a string
    static Value string(String s) {
        Value result = new Value();
        result.type = Type.STRING;

        // We use the `str` member to store the string.
        result.str = s;
        return result;
    }

    // Construct a lambda function
    Value(List<Value> params, Value ret, Environment env) {
        type = Type.LAMBDA;
        // We store the params and the result in the list member
        // instead of having dedicated members. This is to save memory.
        list = new ArrayList<>();
        list.add(new Value(params));
        list.add(ret);

        // Lambdas capture only variables that they know they will use.
        List<String> used_atoms = ret.get_used_atoms();
        for (int i=0; i<used_atoms.size(); i++) {
            // If the environment has a symbol that this lambda uses, capture it.
            if (env.has(used_atoms.get(i)))
                lambda_scope.set(used_atoms.get(i), env.get(used_atoms.get(i)));
        }
    }

    // Construct a builtin function
    Value(String name, Builtin b) {
        type = Type.BUILTIN;
        // Store the name of the builtin function in the str member
        // to save memory, and use the builtin function slot in the union
        // to store the function pointer.
        str = name;
        stack_data = b;
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// C++ INTEROP METHODS ////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////

    // Get all of the atoms used in a given Value
    List<String> get_used_atoms() {
        List<String> result = new ArrayList<>();
        List<String> tmp = new ArrayList<>();
        switch (type) {
            case QUOTE:
                // The data for a quote is stored in the
                // first slot of the list member.
                return list.get(0).get_used_atoms();
            case ATOM:
                // If this is an atom, add it to the list
                // of used atoms in this expression.
                result.add(as_atom());
                return result;
            case LAMBDA:
                // If this is a lambda, get the list of used atoms in the body
                // of the expression.
                return list.get(1).get_used_atoms();
            case LIST:
                // If this is a list, add each of the atoms used in all
                // of the elements in the list.
                for (int i=0; i<list.size(); i++) {
                    // Get the atoms used in the element
                    tmp = list.get(i).get_used_atoms();
                    // Add the used atoms to the current list of used atoms
                    result.addAll(tmp);
                }
                return result;
            default:
                return result;
        }
    }

    // Is this a builtin function?
    boolean is_builtin() {
        return type == Type.BUILTIN;
    }

    boolean is_number() {
        return type == Type.INT || type == Type.FLOAT;
    }

    // Get the "truthy" boolean value of this value.
    boolean as_bool() {
        switch (type) {
            case FLOAT:
                return (double)stack_data != 0.0;
            case INT:
                return (int)stack_data != 0;
            case BUILTIN:
                return stack_data != null;
            case STRING:
            case ATOM:
                // Both atoms and strings store their
                // data in the str member.
                return !str.isEmpty();
            case LAMBDA:
            case LIST:
                // Both lambdas and lists store their
                // data in the list member.
                return !list.isEmpty();
            case QUOTE:
                // The values for quotes are stored in the
                // first slot of the list member.
                return list.get(0).as_bool();
            default:
                return true;
        }
    }

    // Get this item's integer value
    int as_int() {
        return (int)cast_to_int().stack_data;
    }

    // Get this item's floating point value
    double as_float() {
        return (double)cast_to_int().stack_data;
    }

    // Get this item's string value
    String as_string() {
        // If this item is not a string, throw a cast error.
        if (type != Type.STRING)
            throw new Error(this, new Environment(), Error.BAD_CAST);
        return str;
    }

    // Get this item's atom value
    String as_atom() {
        // If this item is not an atom, throw a cast error.
        if (type != Type.ATOM)
            throw new Error(this, new Environment(), Error.BAD_CAST);
        return str;
    }

    // Get this item's list value
    List<Value> as_list() {
        // If this item is not a list, throw a cast error.
        if (type != Type.LIST)
            throw new Error(this, new Environment(), Error.BAD_CAST);
        return list;
    }

    // Push an item to the end of this list
    void push(Value val) {
        // If this item is not a list, you cannot push to it.
        // Throw an error.
        if (type != Type.LIST)
            throw new Error(this, new Environment(), Error.MISMATCHED_TYPES);

        list.add(val);
    }

    // Push an item from the end of this list
    Value pop() {
        // If this item is not a list, you cannot pop from it.
        // Throw an error.
        if (type != Type.LIST)
            throw new Error(this, new Environment(), Error.MISMATCHED_TYPES);

        // Remember the last item in the list
        Value result = list.get(list.size()-1);
        // Remove it from this instance
        list.remove(list.size()-1);
        // Return the remembered value
        return result;
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// TYPECASTING METHODS ////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////

    // Cast this to an integer value
    Value cast_to_int() {
        switch (type) {
            case INT: return new Value((int)stack_data);
            case FLOAT: return new Value((double)((int)stack_data));
            // Only ints and floats can be cast to an int
            default:
                throw new Error(this, new Environment(), Error.BAD_CAST);
        }
    }

    // Cast this to a floating point value
    Value cast_to_float() {
        switch (type) {
            case FLOAT: return new Value((double)stack_data);
            case INT: return new Value((int)((double)stack_data));
            // Only ints and floats can be cast to a float
            default:
                throw new Error(this, new Environment(), Error.BAD_CAST);
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// COMPARISON OPERATIONS //////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////

    boolean eq(Value other) {
        // If either of these values are floats, promote the
        // other to a float, and then compare for equality.
        if (type == Type.FLOAT && other.type == Type.INT) return eq(other.cast_to_float());
        else if (type == Type.INT && other.type == Type.FLOAT) return this.cast_to_float().eq(other);
        // If the values types aren't equal, then they cannot be equal.
        else if (type != other.type) return false;

        switch (type) {
            case FLOAT:
                return (double)stack_data == (double)other.stack_data;
            case INT:
                return (int)stack_data == (int)other.stack_data;
            case BUILTIN:
                return stack_data == other.stack_data;
            case STRING:
            case ATOM:
                // Both atoms and strings store their
                // data in the str member.
                return str.equals(other.str);
            case LAMBDA:
            case LIST:
                // Both lambdas and lists store their
                // data in the list member.
                return list == other.list;
            case QUOTE:
                // The values for quotes are stored in the
                // first slot of the list member.
                return list.get(0) == other.list.get(0);
            default:
                return true;
        }
    }

    boolean neq(Value other) {
        return !eq(other);
    }

    // boolean operator<(Value other) {
    //     if (other.type != Type.FLOAT && other.type != Type.INT)
    //         throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

    //     switch (type) {
    //     case FLOAT:
    //         return stack_data.f < other.cast_to_float().stack_data.f;
    //     case INT:
    //         if (other.type == Type.FLOAT)
    //             return cast_to_float().stack_data.f < other.stack_data.f;
    //         else return stack_data.i < other.stack_data.i;
    //     default:
    //         throw new Error(this, new Environment(), Error.INVALID_ORDER);
    //     }
    // }

    ////////////////////////////////////////////////////////////////////////////////
    /// ORDERING OPERATIONS ////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////

    boolean gte(Value other) {
        return !lt(other);
    }

    boolean lte(Value other) {
        return eq(other) || lt(other);
    }

    boolean gt(Value other) {
        return !lte(other);
    }

    boolean lt(Value other) {
        // Other type must be a float or an int
        if (other.type != Type.FLOAT && other.type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

        switch (type) {
            case FLOAT:
                // If this is a float, promote the other value to a float and compare.
                return (double)stack_data < (double)other.cast_to_float().stack_data;
            case INT:
                // If the other value is a float, promote this value to a float and compare.
                if (other.type == Type.FLOAT)
                    return (float)cast_to_float().stack_data < (float)other.stack_data;
                    // Otherwise compare the integer values
                else return (int)stack_data < (int)other.stack_data;
            default:
                // Only allow comparisons between integers and floats
                throw new Error(this, new Environment(), Error.INVALID_ORDER);
        }
    }

    public boolean even() {
        if (type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_UNARY_OP);
        return (int)(stack_data) % 2 == 0;
    }

    public boolean odd() {
        if (type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_UNARY_OP);
        return (int)(stack_data) % 2 == 1;
    }


    ////////////////////////////////////////////////////////////////////////////////
    /// ARITHMETIC OPERATIONS //////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////

    // This function adds two lisp values, and returns the lisp value result.
    Value add(Value other) {
        // If the other value's type is the unit type,
        // don't even bother continuing.
        // Unit types consume all arithmetic operations.
        if (other.type == Type.UNIT) return other;

        // Other type must be a float or an int
        if ((is_number() || other.is_number()) &&
                !(is_number() && other.is_number()))
            throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

        switch (type) {
            case FLOAT:
                // If one is a float, promote the other by default and do
                // float addition.
                return new Value((double)stack_data + (double)other.cast_to_float().stack_data);
            case INT:
                // If the other type is a float, go ahead and promote this expression
                // before continuing with the addition.
                if (other.type == Type.FLOAT)
                    return new Value((double)cast_to_float().stack_data + (double)other.stack_data);
                    // Otherwise, do integer addition.
                else return new  Value((int)stack_data + (int)other.stack_data);
            case STRING:
                // If the other value is also a string, do the concat
                if (other.type == Type.STRING)
                    return string(str + other.str);
                // We throw an error if we try to concat anything of non-string type
            else throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
            case LIST:
                // If the other value is also a list, do the concat
                if (other.type == Type.LIST) {
                    // Maintain the value that will be returned
                    Value result = clone();
                    // Add each item in the other list to the end of this list
                    for (int i=0; i<other.list.size(); i++)
                        result.push(other.list.get(i));
                    return result;

                } else throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
            case UNIT:
                return clone();
            default:
                throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
        }
    }

    // This function subtracts two lisp values, and returns the lisp value result.
    Value sub(Value other) {
        // If the other value's type is the unit type,
        // don't even bother continuing.
        // Unit types consume all arithmetic operations.
        if (other.type == Type.UNIT) return other;

        // Other type must be a float or an int
        if (other.type != Type.FLOAT && other.type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

        switch (type) {
            case FLOAT:
                // If one is a float, promote the other by default and do
                // float subtraction.
                return new Value((double)stack_data - (double)other.cast_to_float().stack_data);
            case INT:
                // If the other type is a float, go ahead and promote this expression
                // before continuing with the subtraction
                if (other.type == Type.FLOAT)
                    return new Value((double)cast_to_float().stack_data - (double)other.stack_data);
                    // Otherwise, do integer subtraction.
                else return new Value((int)stack_data - (int)other.stack_data);
            case UNIT:
                // Unit types consume all arithmetic operations.
                return clone();
            default:
                // This operation was done on an unsupported type
                throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
        }
    }

    // This function multiplies two lisp values, and returns the lisp value result.
    Value mul(Value other) {
        // If the other value's type is the unit type,
        // don't even bother continuing.
        // Unit types consume all arithmetic operations.
        if (other.type == Type.UNIT) return other;

        // Other type must be a float or an int
        if (other.type != Type.FLOAT && other.type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

        switch (type) {
            case FLOAT:
                return new Value((double)stack_data * (double)other.cast_to_float().stack_data);
            case INT:
                // If the other type is a float, go ahead and promote this expression
                // before continuing with the product
                if (other.type == Type.FLOAT)
                    return new Value((double)cast_to_float().stack_data * (double)other.stack_data);
                    // Otherwise, do integer multiplication.
                else return new Value((int)stack_data * (int)other.stack_data);
            case UNIT:
                // Unit types consume all arithmetic operations.
                return clone();
            default:
                // This operation was done on an unsupported type
                throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
        }
    }

    // This function divides two lisp values, and returns the lisp value result.
    Value div(Value other) {
        // If the other value's type is the unit type,
        // don't even bother continuing.
        // Unit types consume all arithmetic operations.
        if (other.type == Type.UNIT) return other;

        // Other type must be a float or an int
        if (other.type != Type.FLOAT && other.type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

        switch (type) {
            case FLOAT:
                return new Value((double)stack_data / (double)other.cast_to_float().stack_data);
            case INT:
                // If the other type is a float, go ahead and promote this expression
                // before continuing with the product
                if (other.type == Type.FLOAT)
                    return new Value((double)cast_to_float().stack_data / (double)other.stack_data);
                    // Otherwise, do integer multiplication.
                else return new Value((int)stack_data / (int)other.stack_data);
            case UNIT:
                // Unit types consume all arithmetic operations.
                return clone();
            default:
                // This operation was done on an unsupported type
                throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
        }
    }

    // This function finds the remainder of two lisp values, and returns the lisp value result.
    Value mod(Value other) {
        // If the other value's type is the unit type,
        // don't even bother continuing.
        // Unit types consume all arithmetic operations.
        if (other.type == Type.UNIT) return other;

        // Other type must be a float or an int
        if (other.type != Type.FLOAT && other.type != Type.INT)
            throw new Error(this, new Environment(), Error.INVALID_BIN_OP);

        switch (type) {
            // If we support libm, we can find the remainder of floating point values.
//        #ifdef HAS_LIBM
//            case FLOAT:
//                return new Value(fmod(stack_data.f, other.cast_to_float().stack_data.f));
//            case INT:
//                if (other.type == Type.FLOAT)
//                    return new Value(fmod(cast_to_float().stack_data.f, other.stack_data.f));
//                else return new Value(stack_data.i % other.stack_data.i);
//
//        #else
            case INT:
                // If we do not support libm, we have to throw new Errors for floating point values.
                if (other.type != Type.INT)
                    throw new Error(other, new Environment(), Error.NO_LIBM_SUPPORT);
                return new Value((int)stack_data % (int)other.stack_data);
//        #endif

            case UNIT:
                // Unit types consume all arithmetic operations.
                return clone();
            default:
                // This operation was done on an unsupported type
                throw new Error(this, new Environment(), Error.INVALID_BIN_OP);
        }
    }

    // Get the name of the type of this value
    String get_type_name() {
        switch (type) {
            case QUOTE: return QUOTE_TYPE;
            case ATOM: return ATOM_TYPE;
            case INT: return INT_TYPE;
            case FLOAT: return FLOAT_TYPE;
            case LIST: return LIST_TYPE;
            case STRING: return STRING_TYPE;
            case BUILTIN:
            case LAMBDA:
                // Instead of differentiating between
                // lambda and builtin types, we group them together.
                // This is because they are both callable.
                return FUNCTION_TYPE;
            case UNIT:
                return UNIT_TYPE;
            default:
                // We don't know the name of this type.
                // This isn't the users fault, this is just unhandled.
                // This should never be reached.
                throw new Error(this, new Environment(), Error.INTERNAL_ERROR);
        }
    }

    String display() {
        String result = "";
        switch (type) {
            case QUOTE:
                return "'" + list.get(0).debug();
            case ATOM:
                return str;
            case INT:
                return String.valueOf((int)stack_data);
            case FLOAT:
                return String.valueOf((double)stack_data);
            case STRING:
                return str;
            case LAMBDA:
                for (int i=0; i<list.size(); i++) {
                    result += list.get(i).debug();
                    if (i < list.size()-1) result += " ";
                }
                return "(lambda " + result + ")";
            case LIST:
                for (int i=0; i<list.size(); i++) {
                    result += list.get(i).debug();
                    if (i < list.size()-1) result += " ";
                }
                return "(" + result + ")";
            case BUILTIN:
                return "<" + str + " at " + String.valueOf((long)stack_data) + ">";
            case UNIT:
                return "@";
            default:
                // We don't know how to display whatever type this is.
                // This isn't the users fault, this is just unhandled.
                // This should never be reached.
                throw new Error(this, new Environment(), Error.INTERNAL_ERROR);
        }
    }

    String debug() {
        String result="";
        switch (type) {
            case QUOTE:
                return "'" + list.get(0).debug();
            case ATOM:
                return str;
            case INT:
                return String.valueOf((int)stack_data);
            case FLOAT:
                return String.valueOf((double)stack_data);
            case STRING:
                for (int i=0; i<str.length(); i++) {
                    if (str.charAt(i) == '"') result += "\\\"";
                    else result+=str.charAt(i);
                }
                return "\"" + result + "\"";
            case LAMBDA:
                for (int i=0; i<list.size(); i++) {
                    result += list.get(i).debug();
                    if (i < list.size()-1) result += " ";
                }
                return "(lambda " + result + ")";
            case LIST:
                for (int i=0; i<list.size(); i++) {
                    result += list.get(i).debug();
                    if (i < list.size()-1) result += " ";
                }
                return "(" + result + ")";
            case BUILTIN:
                return "<" + str + " at " + String.valueOf((long)stack_data) + ">";
            case UNIT:
                return "@";
            default:
                // We don't know how to debug whatever type this is.
                // This isn't the users fault, this is just unhandled.
                // This should never be reached.
                throw new Error(this, new Environment(), Error.INTERNAL_ERROR);
        }
    }

    void print() {
        System.out.print(display());
    }

    void print(PrintWriter os) {
        os.write(display());
    }

    // Apply this as a function to a list of arguments in a given environment.
    Value apply(List<Value> args, Environment env) {
        Environment e;
        List<Value> params;
        switch (type) {
            case LAMBDA:
                // Get the list of parameter atoms
                params = list.get(0).list;
                if (params.size() != args.size())
                    throw new Error(new Value(args), env, args.size() > params.size()?
                            Error.TOO_MANY_ARGS : Error.TOO_FEW_ARGS
                    );

                // Get the captured scope from the lambda
                e = lambda_scope;
                // And make this scope the parent scope
                e.set_parent_scope(env);

                // Iterate through the list of parameters and
                // insert the arguments into the scope.
                for (int i=0; i<params.size(); i++) {
                    if (params.get(i).type != Type.ATOM)
                        throw new Error(this, env, Error.INVALID_LAMBDA);
                    // Set the parameter name into the scope.
                    e.set(params.get(i).str, args.get(i));
                }

                // Evaluate the function body with the function scope
                return list.get(1).eval(e);
            case BUILTIN:
                // Here, we call the builtin function with the current scope.
                // This allows us to write special forms without syntactic sugar.
                // For functions that are not special forms, we just evaluate
                // the arguments before we run the function.
                Builtin b = (Builtin)stack_data;
                return b.apply(new Param(args, env));
            default:
                // We can only call lambdas and builtins
                throw new Error(this, env, Error.CALL_NON_FUNCTION);
        }
    }

    // Evaluate this value as lisp code.
    Value eval(Environment env) {
        List<Value> args;
        Value function;
        Environment e;
        switch (type) {
            case QUOTE:
                return list.get(0);
            case ATOM:
                return env.get(str);
            case LIST:
                if (list.size() < 1)
                    throw new Error(this, env, Error.EVAL_EMPTY_LIST);

                args = new ArrayList<>(list.subList(1, list.size()));

                // Only evaluate our arguments if it's not builtin!
                // Builtin functions can be special forms, so we
                // leave them to evaluate their arguments.
                function = list.get(0).eval(env);

                if (!function.is_builtin())
                    for (int i=0; i<args.size(); i++)
                        args.set(i, args.get(i).eval(env));

                return function.apply(
                        args,
                        env
                );

            default:
                return clone();
        }
    }


    static boolean is_punct(char ch) {
        return (ch>=33 && ch<=47)||(ch>=58 && ch<=64)||(ch>=91 && ch<=96)
                ||(ch>=123 && ch<=126);
    }


    // Is this character a valid lisp symbol character
    static boolean is_symbol(char ch) {
        return (Character.isAlphabetic(ch) || is_punct(ch)) && ch != '(' && ch != ')' && ch != '"' && ch != '\'';
    }


    // Parse a single value and increment the pointer
// to the beginning of the next value to parse.
    static Value parse(CharScanner lexer) {
        lexer.skip_whitespace();

        while (lexer.peek() == ';') {
            // If this is a comment
            lexer.skip_comments();
            lexer.skip_whitespace();
            if (lexer.eof())
                return new Value();
        }

        if (lexer.eof()) {
            return new Value();
        } else if (lexer.peek() == '\'') {
            // If this is a quote
            lexer.nextChar();
            return quote(parse(lexer));

        } else if (lexer.peek() == '(') {
            // If this is a list
            lexer.nextChar();
            lexer.skip_whitespace();

            Value result = new Value(new ArrayList<>());

            while (lexer.peek() != ')')
                result.push(parse(lexer));

            lexer.nextChar();
            lexer.skip_whitespace();
            return result;

        } else if (Character.isDigit(lexer.peek()) ||
                (lexer.peek() == '-' && Character.isDigit(lexer.peek(1)))) {
            // If this is a number
            boolean negate = lexer.peek() == '-';
            if (negate) lexer.nextChar();

            lexer.setAnchor();
            while (!lexer.eof() && (Character.isDigit(lexer.peek()) || lexer.peek() == '.'))
                lexer.nextChar();
            String n = lexer.getAnchor();
            lexer.skip_whitespace();

            if (n.indexOf('.') >=0)
            return new Value((negate? -1 : 1) * Double.parseDouble(n));
        else return new Value((negate? -1 : 1) * Integer.parseInt(n));

        } else if (lexer.peek() == '\"') {
            // If this is a string
            lexer.nextChar();
            lexer.setAnchor();
            while (lexer.peek() != '\"') {
                lexer.nextChar();
            }

            String x = lexer.getAnchor();
            lexer.nextChar();
            lexer.skip_whitespace();

            // Iterate over the characters in the string, and
            // replace escaped characters with their intended values.
            StringBuilder sbx = new StringBuilder();
            int i = 0;
            while (i<x.length()) {
                if (x.charAt(i) == '\\') {
                    i++;
                    if (x.charAt(i) == '\\')
                        sbx.append("\\");
                    else if (x.charAt(i) == '"')
                        sbx.append("\"");
                    else if (x.charAt(i) == 'n')
                        sbx.append("\n");
                    else if (x.charAt(i) == 't')
                        sbx.append("\t");
                }
                else sbx.append(x.charAt(i));
                i++;
            }
            x = sbx.toString();
            return string(x);
        } else if (lexer.peek() == '@') {
            lexer.nextChar();
            lexer.skip_whitespace();
            return new Value();

        } else if (is_symbol( lexer.peek() )) {
            // If this is a string
            lexer.setAnchor();
            while (is_symbol(lexer.peek())) {
                lexer.nextChar();
            }

            String x = lexer.getAnchor();
            lexer.skip_whitespace();
            return atom(x);
        } else {
            throw new Error(null, new Environment(), Error.MALFORMED_PROGRAM);
        }
    }

// Parse an entire program and get its list of expressions.
    static List<Value> parse(String s) { //todo move to other place?
        s = "  sasd \"123\"6 >12;34.5 \"gh\nhh' +-";
        CharScanner scanner = new CharScanner(s);
        Lexer lexer = new Lexer(scanner);
        while (lexer.next()) {
            Token token = lexer.peek();
            System.out.printf("%s %s\n", token.type.toString(), token.value);
        }

        System.exit(0);
        return null;
/*
        List<Value> result = new ArrayList<>();
        // While the parser is making progress (while the pointer is moving right)
        // and the pointer hasn't reached the end of the string,
        while (scanner.peek()!=0) {
            // Parse another expression and add it to the list.
            //last_i = i[0];
            result.add(parse(scanner));
        }

        // If the whole string wasn't parsed, the program must be bad.
        if (!scanner.eof())
            throw new Error(null, new Environment(), Error.MALFORMED_PROGRAM);

        // Return the list of values parsed.
        return result;*/
    }

    // Execute code in an environment
    static Value run(String code, Environment env) { //todo here?
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
