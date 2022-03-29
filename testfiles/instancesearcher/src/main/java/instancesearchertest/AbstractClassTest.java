package instancesearchertest;

public abstract class AbstractClassTest {
    // Shouldn't show up, since abstract class
    public AbstractClassTest() {
    }

    public static AbstractClassTest staticField;
    public AbstractClassTest nonStaticField;
    private static AbstractClassTest privateStaticField;

    public static AbstractClassTest staticMethod() {
        return null;
    }
}
