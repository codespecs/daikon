package MapQuick.tigerdb;

/**
 * NonNumericDataException.java
 *
 *
 * Created: Wed Aug 16 05:05:12 2000
 *
 * @author Felix S. Klock II
 */

public class NonNumericDataException extends BadRecordException {
    
  public NonNumericDataException(String o) {
    super("non-numeric data", o);
  }
    
} // NonNumericDataException
