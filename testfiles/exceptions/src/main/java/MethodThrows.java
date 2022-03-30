import java.io.IOException;

public class MethodThrows {
    public static void failWithError() {
        throw new java.lang.Error();
    }

    public static void failWithRuntimeException() {
        throw new RuntimeException();
    }

    public static void failWithDeclaredException() throws IOException {
        throw new IOException();
    }
}
