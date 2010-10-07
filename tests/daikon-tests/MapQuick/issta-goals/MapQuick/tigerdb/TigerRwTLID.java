package MapQuick.tigerdb;

/**
 * TigerRwTLID.java
 *
 *
 * Created: Wed Aug 16 00:49:54 2000
 *
 * @author Felix S. Klock II
 */

public class TigerRwTLID extends TigerRecord {
  public final int tlid;
    
  public TigerRwTLID(String s, int size, int tlidStart) 
    throws BadRecordException {
    super(s, size);
    try {
      tlid = toInt(s.substring(tlidStart,tlidStart+10));
    } catch (NoInt e) {
      throw new RuntimeException();
    }
  }

  public TigerRwTLID(String s, int size) throws BadRecordException {
    this(s, size, 5);
  }
    
} // TigerRwTLID
