package instancesearchertest;

public interface InterfaceTest {
    static interface SubInterface extends InterfaceTest {}

    class ClassImplementingSubInterface implements SubInterface {
    }

    class ClassImplementingInterface implements InterfaceTest {
    }

    static ClassImplementingSubInterface instanceOfClass = null;
    static InterfaceTest instanceOfInterface = null;
}
