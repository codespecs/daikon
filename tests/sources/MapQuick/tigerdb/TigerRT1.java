package MapQuick.tigerdb;

import MapQuick2.GeoPoint;

/**
 * TigerRT1 represents a Complete-Chain Basic Data Record. 
 *
 *
 * Created: Tue Aug 15 19:06:15 2000
 *
 * @author Felix S. Klock II
 */
public class TigerRT1 extends TigerRwTLID {
  public GeoPoint from, to;
  public Feature feature;

  public  DirectedStreetNumberRange lftRange;
  public  DirectedStreetNumberRange rgtRange;

  public  String cfc;

  public String lftZip;        
  public String rgtZip;        

  public String toString() {
    if (true) 
      return 
	"TigerRT1< "+from+", "+to+
	", "+feature+
	", "+lftRange+", "+rgtRange+
	", "+lftZip+", "+rgtZip+
	" >";
    else
      return "< "+from+","+to+">";
  }

  /** Contructs a TigerRT1 from a line of Record Type 1 as specified
   * in the Data Dictionary for the Tiger/Line files.
   */
  public TigerRT1(String s) throws BadRecordException {
    super(s, 228);
    if (s.charAt(0) != '1')
      die("RT1 record type must be 1, not "+s.charAt(0));

    int lat1, lng1, lat2, lng2;
    lat1 = lng1 = lat2 = lng2 = 0;
    try {
      lat1 = toInt(s.substring(190,200));
      lng1 = toInt(s.substring(200,209));
      lat2 = toInt(s.substring(209,219));
      lng2 = toInt(s.substring(219,228));
      // from = new GeoPoint(div1M(toInt(s.substring(190,200))),
      //		div1M(toInt(s.substring(200,209)))).intern();
      // to   = new GeoPoint(div1M(toInt(s.substring(209,219))),
      //		div1M(toInt(s.substring(219,228)))).intern();
      if (lat1 == lat2 && lng1 == lng2) {
	// System.out.println("XXX "+s + "\nXXX is zero length");
	throw new BadRecordException(s, "zero length geosegment!");
      }
    } catch (NoInt e) {
      throw new RuntimeException
	(s.substring(190,228)+ " should contain two Points");
    }
    from = makeGP(lat1, lng1);
    to   = makeGP(lat2, lng2);

    // Do not call Feature.intern(), since Feature is short-lived,
    // and doesn't intern well
    feature = new Feature(s.substring(17, 55));
	
    cfc = s.substring(55,58);
	
    lftRange = parseAddrRange(s.substring(58, 69), s.substring(69, 80));
    rgtRange = parseAddrRange(s.substring(80, 91), s.substring(91, 102));

    lftZip = s.substring(106, 111).intern();
    rgtZip = s.substring(111, 116).intern();
  }



  /** Returns the primary name of this, or "" if this does not have
   * a primary name.  Right now the name contains the Primary
   * Feature PrefixDirection, Name, Type, and SuffixDirection. 
   */
  public String primaryName() { return feature.fullName(); }

  public boolean hasAddress(int a) {
    if (lftRange != null && lftRange.contains(a))
      return true;
    if (rgtRange != null && rgtRange.contains(a))
      return true;
    return false;
  }
} // TigerRT1
