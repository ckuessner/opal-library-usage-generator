public abstract class AbstractClassThatThrowsInStaticInitializer {
    static {
        if (true) throw new RuntimeException();
    }
}
