package MapQuick.tigerdb;

import java.io.*;
import java.util.*;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;

public class DatabaseReader {

  public static boolean INFO = false;

  // private Map type1 = new HashMap(); // TLID -> TigerRT1
  private Map type1 = new TreeMap(); // TLID -> TigerRT1
  private ToBag type6 = new ToBag(); // TLID ->* TigerRT6

  // tracks tlids that we've thrown out as useless for our purposes
  // (b/c their TigerRT1's weren't Roads, they were Rails or
  // something...)
  private Set trashTLIDS = new HashSet(); 

  public class GeoChain {
    public final TigerRT1 rt1;
    public final Collection rt6s; // Set[TigerRT2]
    GeoChain(TigerRT1 rt1, Collection rt6s) {
      this.rt1  = rt1;
      this.rt6s = rt6s;
    }
  }

  // @effects  Returns an Iterator[GeoChain] over the GeoChains currently
  //           stored in this
  // @mandates this is not modified while the returned iterator is
  //           in use
  public Iterator geoChains() {
    return new ImmIterator() {
	Iterator tlids = type1.keySet().iterator();
	public boolean hasNext() {
	  return tlids.hasNext();
	}
	public Object next() {
	  Object tlid = tlids.next();
	  TigerRT1 rt1 = (TigerRT1) type1.get(tlid);

	  Collection rt6s = type6.getBag(tlid);
	  rt6s = Collections.unmodifiableCollection(rt6s);

	  return new GeoChain(rt1, rt6s);
	}
      };
  }

  public Iterator geoChains(File zf) throws IOException { 
    return geoChains(new ZipFile(zf));
  }
  public Iterator geoChains(String zf) throws IOException { 
    return geoChains(new ZipFile(zf));
  }
  private Iterator geoChains(ZipFile zf) {
    // prep by reading type6 records first
    try {
      Enumeration entries = zf.entries();
      while(entries.hasMoreElements()) {
	ZipEntry entry = (ZipEntry) entries.nextElement();
	if (entry.getName().toLowerCase().endsWith("rt6")) {
	  this.readRecords(zf.getInputStream(entry));
	  break;
	}
      }
	    
      entries = zf.entries();
      ZipEntry mainEntry = null;
      while(entries.hasMoreElements()) {
	ZipEntry entry = (ZipEntry) entries.nextElement();
	if (entry.getName().toLowerCase().endsWith("rt1")) {
	  mainEntry = entry;
	  break;
	}
      }
      if (mainEntry == null) {
	return new ImmIterator() {
	    public boolean hasNext() { return false; }
	    public Object next() { 
	      throw new NoSuchElementException();
	    }
	  };
      }
      final LineNumberReader lnr = 
	new LineNumberReader
	  (new InputStreamReader(zf.getInputStream(mainEntry))); 
	    
      // now return the nifty read-and-throw-away GeoChain iterator 
      return new ImmIterator() {
	  TigerRT1 rt1 = null;
	  private void advance() {
	    while (rt1 == null) {
	      String line;
	      try {
		line = lnr.readLine();
	      } catch (IOException ioe) {
		line = null;
	      }
	      if (line == null) {
		return;
	      }
	      try {
		rt1 = new TigerRT1(line);
		if (rt1.cfc.charAt(0) != 'a' &&
		    rt1.cfc.charAt(0) != 'A') {
		  trashTLIDS.add(new Integer(rt1.tlid));
		  type6.remove(new Integer(rt1.tlid));
				// System.out.println("1: ("+type1.size()+") Skipping "+rt1);
		  rt1 = null;
		}
	      } catch (BadRecordException bre) {
		// System.out.println("bad record: " + bre.getMessage());
		rt1 = null;
	      }
	    }
	  }
	  public boolean hasNext() {
	    advance(); 
	    return (rt1 != null);
	  }
	  public Object next() {
	    GeoChain gc = 
	      new GeoChain(rt1,type6.getBag(new Integer(rt1.tlid)));
	    rt1 = null;
	    advance();
	    return gc;
	  }
	};
    } catch (IOException ioe) {
      throw new RuntimeException(ioe.getMessage());
    }
  }

  public DatabaseReader() {

  }

  // checks internal state of this to increase confidence that the
  // data set isn't screwy 
  public void checkMappingInv() {
    checkOneMappingInv(type6);
  }

  // checks that every key in typeT has an RT1 in type1
  private void checkOneMappingInv(Map typeT) {
    for(Iterator tki = typeT.keySet().iterator();tki.hasNext();) {
      Integer tlid = (Integer) tki.next();
      if (false && !type1.containsKey(tlid)) 
	System.out.println("!!! No record found for tlid:"+tlid+
			   " with records:"+ typeT.get(tlid));
    }
  }

  public static void main(String[] args) {
    DatabaseReader db = new DatabaseReader();

    // each arg is a ZIP file
    for(int i=0; i<args.length; i++) {
      String zfstr = args[i];
      try {
	db.readZipFile(zfstr);
      } catch (IOException e) {
	System.out.println("IOEXCEPTION?");
		
      } catch (OutOfMemoryError e) {
		
	System.out.println("OUT OF MEMORY");

	System.out.println("1 "+db.type1.size());
	System.out.println("6 "+db.type6.size());
		
	e.printStackTrace();
	System.exit(-1);
      }
    }

	
    db.checkMappingInv();
	
    if (false) {  
      // Prints Feature.fullNames to STDOUT 
      // (when fullNames is supported) 

      Iterator names = null;
      // names = Feature.fullNames.iterator();
      for(int i=0; names.hasNext(); i++){
	String name = (String) names.next();
	System.out.println(i+"\t"+name);
      }
    }
  }

  public void readZipFile(File zf) throws IOException {
    readZipFile(new ZipFile(zf));
  }

  public void readZipFile(String zstr) throws IOException {
    readZipFile(new ZipFile(zstr));
    if (INFO) 
      System.out.println("finished with "+zstr);
  }
    
  private void readZipFile(ZipFile zf) throws IOException {
    Enumeration entries = zf.entries();
    // build the type6 map for 
    while(entries.hasMoreElements()) {
      ZipEntry entry = (ZipEntry) entries.nextElement();
      this.readRecords(zf.getInputStream(entry));
    }
    zf.close();
  }

  public void readRecords(InputStream is) throws IOException {
    LineNumberReader lnr = new LineNumberReader(new InputStreamReader(is));
    for(String line=lnr.readLine(); line!=null; line=lnr.readLine() ) {
      // System.out.print("*");
      try {
	switch(line.charAt(0)) {
	case '1': 
	  TigerRT1 rt1 = new TigerRT1(line);
		    
	  if (rt1.cfc.charAt(0) != 'a' && 
	      rt1.cfc.charAt(0) != 'A') {
	    trashTLIDS.add(new Integer(rt1.tlid));
	    type6.remove(new Integer(rt1.tlid));
	    // System.out.println("1: ("+type1.size()+") Skipping "+rt1);
	  } else {
	    if (type1.containsKey(new Integer(rt1.tlid))) 
	      throw new RuntimeException("1: SOMETHING'S WRONG");
	    // System.out.println("1: ("+type1.size()+") Adding "+rt1);
	    type1.put(new Integer(rt1.tlid), rt1);
	  }
	  break;
	case '6': 
	  TigerRT6 rt6 = new TigerRT6(line);
	  if (!trashTLIDS.contains(new Integer(rt6.tlid))) {
	    type6.getBag(new Integer(rt6.tlid)).add(rt6);
	    // System.out.println("6: Adding "+rt6);
	  } else {
	    // System.out.println("6: Skipping "+rt6);
	  }
	  break; 
	default:
	  // System.out.println("skipping record, type: "+line.charAt(0));
	}
      } catch (BadRecordException e) {
	// Skip record
	if (false) {
	  System.out.println("BAD RECORD !!!");
	  System.out.println(e.reason());
	  System.out.println();
	  if (true) {
	    System.out.println
	      ("000000000111111111122222222223"+
	       "333333333444444444455555555556"+
	       "66666666677777777778");
	    System.out.println
	      ("123456789012345678901234567890"+
	       "123456789012345678901234567890"+
	       "12345678901234567890");
	  }
	  System.out.println(line);
	  System.out.println();
	}

		
      }
      // System.gc();
    }
  }
}
