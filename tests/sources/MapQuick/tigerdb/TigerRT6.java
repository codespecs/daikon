package MapQuick.tigerdb;

import java.util.*;

/**
 * TigerRT6.java
 *
 *
 * Created: Wed Aug 16 02:14:16 2000
 *
 * @author Felix S. Klock II
 */
public class TigerRT6 extends TigerRwTLID {
  public int rtsq;
  public DirectedStreetNumberRange lftRange;
  public DirectedStreetNumberRange rgtRange;
    
  public TigerRT6(String s) throws BadRecordException {
    super(s, 76);
    try { rtsq = toInt(s.substring(15,18));
    } catch (NoInt e) { die(""); }

    lftRange = parseAddrRange(s.substring(18,29), s.substring(29,40));
    rgtRange = parseAddrRange(s.substring(40,51), s.substring(51,62));
  }
    
} // TigerRT6
