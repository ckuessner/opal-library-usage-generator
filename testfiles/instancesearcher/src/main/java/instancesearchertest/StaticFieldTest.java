package instancesearchertest;

public class StaticFieldTest {
    private StaticFieldTest() {
    }

    public static StaticFieldTest publicStaticField;
    protected static StaticFieldTest protectedStaticField;
    static StaticFieldTest staticDefaultField;

    // Shouldn't show up since non-static
    StaticFieldTest nonStaticField;
    // Shouldn't show up since private
    private static StaticFieldTest privateStaticField;
}
