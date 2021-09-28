// An exception thrown by the lisp
public class Error extends java.lang.Error {
    static final String TOO_FEW_ARGS = "too few arguments to function";
    static final String TOO_MANY_ARGS = "too many arguments to function";
    static final String INVALID_ARGUMENT = "invalid argument";
    static final String MISMATCHED_TYPES = "mismatched types";
    static final String CALL_NON_FUNCTION = "called non-function";
    static final String UNKNOWN_ERROR = "unknown exception";
    static final String INVALID_LAMBDA = "invalid lambda";
    static final String INVALID_BIN_OP = "invalid binary operation";
    static final String INVALID_ORDER = "cannot order expression";
    static final String BAD_CAST = "cannot cast";
    static final String ATOM_NOT_DEFINED = "atom not defined";
    static final String EVAL_EMPTY_LIST = "evaluated empty list";
    static final String INTERNAL_ERROR = "interal virtual machine error";
    static final String INDEX_OUT_OF_RANGE = "index out of range";
    static final String MALFORMED_PROGRAM = "malformed program";
    static final String NO_LIBM_SUPPORT = "no libm support";

    Value cause;
    Environment env;
    String msg;

    // Create an error with the value that caused the error,
    // the scope where the error was found, and the message.

    Error(Value v, Environment env, String msg) {
        this.env = env;
        this.msg = msg;
        cause = v.clone();
    }

//    Error(Error other)  {
//        this.env = other.env;
//        this.msg = other.msg;
//        cause = new Value(other.cause);
//    }

    String description() {
        return "error: the expression `" + cause.debug() + "` failed in scope " + env.toString() + " with message \"" + msg + "\"";
    }

}
