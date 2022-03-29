package instancesearchertest;

public class ClassThatUsesAllOtherClasses {
    // Avoid adding constructor to instance sources
    private ClassThatUsesAllOtherClasses() {
    }

    public static void takesParameters(AbstractClassTest a,
                                       ConstructorTest b,
                                       InheritanceTest c,
                                       InnerClassTest d,
                                       StaticFieldTest e,
                                       StaticMethodTest f,
                                       InterfaceTest.SubInterface g

    ) {
    }
}
