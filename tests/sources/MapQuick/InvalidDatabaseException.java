package MapQuick;

public class InvalidDatabaseException extends Exception {
    public InvalidDatabaseException() {
	super();
    }
    public InvalidDatabaseException (String dbname) {
	super (dbname);
    }
}
    
