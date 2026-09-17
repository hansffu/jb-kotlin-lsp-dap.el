package demo;

/** A tiny dependency with a recognizable declaration and documentation. */
public class Greeter {
    /** Return a greeting. */
    public String greet() {
        return "Hello from the dependency";
    }

    @Override
    public String toString() {
        return greet();
    }
}
