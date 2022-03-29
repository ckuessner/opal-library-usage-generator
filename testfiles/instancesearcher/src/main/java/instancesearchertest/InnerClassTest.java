package instancesearchertest;

public class InnerClassTest {
    static class StaticInnerClass {
        static StaticFieldTest staticField;

        protected static StaticFieldTest staticMethod() {
            return null;
        }

        // Shouldn't show up
        private static StaticFieldTest privateStaticField;

        private static StaticFieldTest privateStaticMethod() {
            return null;
        }
    }

    // Shouldn't show up, since private class
    private static class PrivateStaticInnerClassOfA {
        public static StaticFieldTest staticInstanceProvider;
    }
}
