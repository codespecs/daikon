package MapQuick;

public class InvalidAddressException extends Exception {
    public InvalidAddressException () {
        super ();
    }
    /*@ requires s != null; */
    public InvalidAddressException(String s) {
        super(s);
    }
} 