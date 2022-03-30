public class StaticInitializerThrows {
    static {
        initializerMethodThatFails();
    }

    public void callMe() {}

    private static void initializerMethodThatFails() {
        throw new RuntimeException();
    }
}
