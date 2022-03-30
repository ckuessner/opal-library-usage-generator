public class StaticInitializerThrowsWithStaticMethod {
    static {
        initializerMethodThatFails();
    }

    public static void callMe() {
    }

    private static void initializerMethodThatFails() {
        throw new RuntimeException();
    }
}
