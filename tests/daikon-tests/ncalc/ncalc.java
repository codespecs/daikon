public class ncalc {
    // Load the native library
    static {
        System.loadLibrary("calculator");
    }

    // Declare a native method that takes two integers and returns an integer
    public native int add(int a, int b);

    public static void main(String[] args) {
        ncalc calculator = new ncalc();
        int result = calculator.add(10, 5); // Calling the native method with parameters
        System.out.println("Result: " + result);
    }
}
