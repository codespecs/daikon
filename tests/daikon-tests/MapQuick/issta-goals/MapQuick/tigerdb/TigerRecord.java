package MapQuick.tigerdb;
 
import java.io.Serializable;
import java.util.HashMap;
import MapQuick2.GeoPoint;

/**
 * TigerRecord.java
 *
 *
 * Created: Tue Aug 15 19:02:09 2000
 *
 * @author Felix S. Klock II
 */

public class TigerRecord implements Serializable {

  private static final HashMap internGP = null; // new HashMap();
  protected static GeoPoint makeGP(int lng, int lat) { 
    GeoPoint gp = new GeoPoint(lat, lng);
    if (true) return gp; 
    if (!internGP.containsKey(gp)) {
      internGP.put(gp, gp);
    }
    return (GeoPoint) internGP.get(gp);
  }

  /** Special ctor to work around predefined record length (Record
   * Type C seems to be flawed. 
   */ 
  public TigerRecord() { }

  public TigerRecord(String s, int recordSize) {
    if (s.length() != recordSize)
      die("length must be "+recordSize+
	  ", not "+s.length());
  }
    
  protected void die(String reason) {throw new RuntimeException(reason);}

  /** Parses `s' as an int, with the following additions to the
   *  standard Integer.parseInt() method:
   *  1. White space around `s' is allowed (and ignored)
   *  2. If s is an empty string after trimming, throws NoInt
   *  3. The number may be prefixed by a '+' character.
   *  4. If s is contains non-numeric data, throws NonNumericDataException
   */
  protected int toInt(String s) throws NoInt, NonNumericDataException {
    String sorig = s;
    s = s.trim();
    if (s.length() == 0) throw new NoInt();
    if (s.charAt(0) == '+') {  
      if (false && s.length()==5) 
	System.out.println("Saw + in "+s);
      s = s.substring(1);
    }
    try {
      return Integer.parseInt(s);
    } catch (NumberFormatException e) {
      throw new NonNumericDataException(s);
    }
  }

  protected static class NoInt extends Throwable { }

  public DirectedStreetNumberRange
    parseAddrRange(String frStr, String toStr) throws BadRecordException {

    try {
      int fraddl = toInt(frStr);
      int toaddl = toInt(toStr);
      if ((fraddl % 2) != (toaddl % 2)) 
	throw new BadRecordException
	  ("bad left address ["+fraddl+","+toaddl+"]",frStr+toStr);
      int low =  Math.min(fraddl, toaddl);
      int high = Math.max(fraddl, toaddl);
	    
      boolean b = (fraddl < toaddl);
      IntSet s = new IntSet(low, high);
      return new DirectedStreetNumberRange(s, b);
    } catch (NoInt e) {
      return new DirectedStreetNumberRange();
    } catch (NumberFormatException e) {
      throw new NonNumericDataException(frStr+","+toStr);
    }
  }


} // TigerRecord
