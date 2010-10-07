package MapQuick.tigerdb;

/**
 * BadRecordException.java
 *
 *
 * Created: Wed Aug 16 04:51:12 2000
 *
 * @author Felix S. Klock II
 */

public class BadRecordException extends Exception {
  String r, o;
  public BadRecordException(String reason, String origData) {
    r = reason;
    o = origData;
  }

  public String reason() { return r; }
  public String originalData() { return o; }
    
} // BadRecordException
