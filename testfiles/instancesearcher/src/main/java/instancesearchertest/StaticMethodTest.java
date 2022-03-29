package instancesearchertest;

public class StaticMethodTest {
    private StaticMethodTest() {
    }

    public static StaticMethodTest getInstance() {
        return new StaticMethodTest();
    }

    static StaticMethodTest getInstance(int a) {
        return new StaticMethodTest();
    }

    protected static StaticMethodTest getInstance(boolean b) {
        return new StaticMethodTest();
    }

    private static StaticMethodTest getInstance(char c) {
        return new StaticMethodTest();
    }
}
