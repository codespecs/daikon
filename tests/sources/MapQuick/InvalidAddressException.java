package MapQuick;

public class InvalidAddressException extends Exception {
    public InvalidAddressException () {
	super ();
    }
    public InvalidAddressException(String s) {
	super(s);
    }
}
