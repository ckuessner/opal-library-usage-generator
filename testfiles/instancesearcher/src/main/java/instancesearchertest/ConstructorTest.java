package instancesearchertest;

public class ConstructorTest {
    public ConstructorTest() {
    }

    public ConstructorTest(ConstructorTest c) {
    }

    protected ConstructorTest(int b) {
    }

    ConstructorTest(float c) {
    }

    // Should not show up
    private ConstructorTest(char a) {
    }
}
