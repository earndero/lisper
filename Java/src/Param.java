import java.util.List;

public class Param {
    List<Value> args;
    Environment env;
    Param(List<Value> args, Environment env) {
        this.args = args;
        this.env = env;
    }
}
