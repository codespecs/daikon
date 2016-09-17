package MapQuick;

import MapQuick2.*;


import java.io.*;
import java.util.*;

import MapQuick.tigerdb.*;
import MapQuick.tigerdb.DatabaseReader.GeoChain;

/**
 * TODO
 */
public class StreetSegIterator
  extends ImmIterator
{
  /** When true, zero-length street segments will be filtered out.  Default is true. */
  public boolean filter_zero_length = true;

  /** When true, filtering will be performed as determined by the killfile.  Default is true. */
  public boolean filter_killfile = true;

  /** When true, progress messages are sent to System.err as segments are read.  Default is false. */
  public boolean mention_progress = false;

  /** When true, warning messages are seng to System.err if segments are filtered.  Default is false. */
  public boolean mention_filter = false;

  /** When true, warning messages are seng to System.err if numbers are not disjoint.  Default is false. */
  public boolean mention_non_disjoint = false;

  /**
   * @requires files != null &&
   *           elements of files are of type java.io.File &&
   *           elements of files are .zip files
   * 
   * @effects creates a new iterator over which produces segments read
   *          in from the given files
   */
  public StreetSegIterator(Iterator files, StreetSegmentFilter filter)
  {
    this.files = files;
    this.filter = filter;
  }

  private boolean initialized = false;
    
  private StreetSegmentFilter filter; // filter which lets call provide a way to filter results
  private Iterator files;     // .zip files to be read, elements are java.io.File
  private Iterator chains;    // chains from the current file, elements are tigerdb.GeoChain
  private StreetSegment next; // next segment to be returned, or null if there are no more
  private long total = 0;     // number of segments returned so far

  public boolean hasNext()
  {
    if (!initialized) {
      initialized = true;
      next = nextSegment();
    }

    return (next != null);
  }

  public Object next()
  {
    // standard iterator behavior
    if (!hasNext()) {
      throw new NoSuchElementException();
    }

    // grab the segment to be returned, then advance to the next one
    Object result = next;
    next = nextSegment();

    // instrument reading process, because it's a bit slow
    total++;
    if (mention_progress && ((total % 10000) == 0)) {
      System.err.println("Returning "+total+"th StreetSegment");
      System.err.flush();
    }

    return result;
  }

  // @return the next segment from the files (post-filtering), or null if none exist
  private StreetSegment nextSegment()
  {
    // grab the next chain from the file
    GeoChain chain = nextChain();
    if (chain == null) {
      return null;
    }

    // make a segment from it
    StreetSegment candidate = makeSegment(chain);

    // if segment could not be made, try again
    if (candidate == null) {
      return nextSegment();
    }

    // if segment doesn't pass the filter, try again
    if (filter_killfile && !filter.apply(candidate)) {
      if (mention_filter) {
	System.err.println("Filtered out: " + candidate);
      }
      return nextSegment();
    }

    // otherwise, it was a good segment
    return candidate;
  }

  // @return the next GeoChain contained in the file(s)
  private GeoChain nextChain()
  {
    // return a chain if we have one ...
    if (chains != null && chains.hasNext()) {
      return (GeoChain) chains.next();
    }

    // else, advance to the next file...
    if (!files.hasNext()) {
      return null;
    }
    File fileToRead = (File) files.next();
    if (mention_progress) {
      System.err.println("Reading from " + fileToRead);
      System.err.flush();
    }

    // ... and open it ...
    try {
      DatabaseReader dr = new DatabaseReader();
      chains = dr.geoChains(fileToRead);
    } catch (IOException ioe) {
      throw new RuntimeException("IOException: " + ioe.getMessage());
    }

    // ... and try again
    return nextChain();
  }

  // @return a segment created from the chain, or null if the
  // segment is not desirable
  private StreetSegment makeSegment(GeoChain chain)
  {
    GeoPoint p1 = chain.rt1.from;
    GeoPoint p2 = chain.rt1.to;
    String name = chain.rt1.feature.fullName();

    if (filter_zero_length && p1.equals(p2)) {
      if (mention_filter) {
	System.err.println("Filtered out zero-length segment named " + name);
	System.err.flush();
      }
      return null;
    }

    IntSet lftAddr = getLftAddresses(chain);
    IntSet rgtAddr = getRgtAddresses(chain);
    if (!lftAddr.isDisjoint(rgtAddr)) {
      if (mention_non_disjoint) {
	System.err.println("Numbers on " + name + " were not disjoint, so were changed to empty sets");
	System.err.flush();
      }
      rgtAddr = lftAddr = new IntSet();
    }

    StreetNumberSet leftSns = makeSNS(lftAddr);
    StreetNumberSet rightSns = makeSNS(rgtAddr);

    String leftZip = chain.rt1.lftZip;
    String rightZip = chain.rt1.rgtZip;

    StreetClassification streetClass = getStreetClass(chain);
    boolean incAddr = areAddressesIncreasing(chain);

    return new StreetSegment(p1, p2, name.intern(), leftSns, rightSns,
			     leftZip, rightZip, streetClass, incAddr);
  }

  private static IntSet getLftAddresses(GeoChain gc) {
    IntSet set = gc.rt1.lftRange.s;
    for(Iterator r6s = gc.rt6s.iterator(); r6s.hasNext();){
      TigerRT6 rt6 = (TigerRT6) r6s.next();
      set = set.union(rt6.lftRange.s);
    }
    return set;
  }

  private static IntSet getRgtAddresses(GeoChain gc) {
    IntSet set = gc.rt1.rgtRange.s;
    for(Iterator r6s = gc.rt6s.iterator(); r6s.hasNext();){
      TigerRT6 rt6 = (TigerRT6) r6s.next();
      set = set.union(rt6.rgtRange.s);
    }
    return set;
  }

  private static final StreetNumberSet EMPTY_SNS = new StreetNumberSet("");
  private static StreetNumberSet makeSNS(IntSet s)
  {
    if (s.size() == 0) return EMPTY_SNS;
    return new StreetNumberSet(s.unparse());
  }

  private static StreetClassification getStreetClass(GeoChain gc) {
    String s = gc.rt1.cfc.toLowerCase();
	
    if (s.charAt(0) == 'a' || s.charAt(0) == 'A') {
      switch (s.charAt(1)) {
      case '1':
      case '2':
	return StreetClassification.PRIM_HWY;
      case '3':
	return StreetClassification.SEC_HWY;
      case '4':
	return StreetClassification.LOCAL_ROAD;
      default:
	return StreetClassification.UNKNOWN;
      }
    } else {
      return StreetClassification.UNKNOWN;
    }
  }

  private static boolean areAddressesIncreasing(GeoChain gc) {
    if (gc.rt1.lftRange.size() > 1 &&
	gc.rt1.rgtRange.size() > 1)
      if (!gc.rt1.lftRange.sameDir(gc.rt1.rgtRange)) {
	// this is too harsh
	if (false) 
	  throw new RuntimeException("inconsistent address deltas "+
				     "tlid:"+gc.rt1.tlid+" "+
				     gc.rt1.lftRange+" "+gc.rt1.rgtRange);
	if (false)
	  System.out.println
	    ("ambiguous address direction for "+gc.rt1.primaryName());
      }
	
    return gc.rt1.lftRange.couldBeLowToHigh();
  }
}
