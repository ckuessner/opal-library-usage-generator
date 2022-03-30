public class ConstructorThrows {
    public ConstructorThrows() {
        throw new RuntimeException();
    }

    public ConstructorThrows(int p) throws Exception {
        throw new Exception();
    }

    public void useMe() {}
}
