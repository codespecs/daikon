package MapQuick;

import MapQuick2.*;


import java.io.*;
import java.util.*;

/**
 * Abstraction to read and return filters based on killfiles
 */
public class KillfileReader
{
  private static boolean debug = false;
  private static void debugln(String s) { if (debug) { System.err.println("KFR: " + s); } }

  /**
   * @return a filter which passes segments not listed in the given killfile
   */
  public static StreetSegmentFilter fromFile(File killfile)
  {
    debugln("Reading " + killfile);
    return new KillfileFilter(killfile);
  }

  /**
   * @return a filter which passes segments not listed in $(dbdir)/*killfile.txt
   */
  public static StreetSegmentFilter fromDir(File dbdir)
  {
    if (!dbdir.isDirectory()) {
      throw new KillfileException("Not a directory: " + dbdir);
    }

    File[] files = dbdir.listFiles(killfile_filter);
    if (files.length == 0) {
      return new AllPassStreetSegmentFilter();
    }
    Arrays.sort(files);

    StreetSegmentFilter result = fromFile(files[0]);
    for (int i=1; i<files.length; i++) {
      StreetSegmentFilter another = fromFile(files[i]);
      result = new CompositeStreetSegmentFilter(result, another);
    }

    return result;
  }

  // filter which gives us *killfile.txt files
  private static final FilenameFilter killfile_filter = new FNFilter();
  private static class FNFilter
    implements FilenameFilter
  {
    public boolean accept(File d, String name) {
      return (name.toLowerCase().endsWith("killfile.txt"));
    }
  }

  /**
   * Indicates that a killfile was non-existant, malformed, etc.
   */
  public static class KillfileException
    extends RuntimeException
  {
    public KillfileException(String s) { super(s); }
  }

  /**
   * Filter which allows all segments through
   */
  private static class AllPassStreetSegmentFilter
    implements StreetSegmentFilter
  {
    public boolean apply(StreetSegment seg)
    {
      return true;
    }
  }

  /**
   * Filter which composes two other filters
   */
  private static class CompositeStreetSegmentFilter
    implements StreetSegmentFilter
  {

    public CompositeStreetSegmentFilter(StreetSegmentFilter one, StreetSegmentFilter two)
    {
      this.one = one;
      this.two = two;
    }

    private final StreetSegmentFilter one;
    private final StreetSegmentFilter two;

    public boolean apply(StreetSegment seg)
    {
      return one.apply(seg) && two.apply(seg);
    }
  }

  /**
   * Filter which filters based on some killfile
   */
  private static class KillfileFilter
    implements StreetSegmentFilter
  {

    public boolean apply(StreetSegment seg)
    {
      if (kill.contains(seg)) {
	KillfileReader.debugln("Killed " + seg);
	return false;
      }

      if (limit_one.contains(seg)) {
	KillfileReader.debugln("First hit on " + seg);
	limit_one.remove(seg);
	kill.add(seg);
      }

      return true;
    }

    private final Set kill = new LinkedHashSet();
    private final Set limit_one = new LinkedHashSet();


    // not a real field, but want scope in c-tor and helper
    private String line;
    private String orig_line;

    private KillfileFilter(File killfile)
    {
      try {

	BufferedReader lines = new BufferedReader(new InputStreamReader(new FileInputStream(killfile)));
	while ((orig_line = line = lines.readLine()) != null) {
	  // e.g. "KILL$(unnamed street)$41271599$-70181563$41273016$-70172648$1$$$     $     $UNKNOWN"

	  String command = nextToken();
	  String name = nextToken();
	  int p1_lat = parseInt(nextToken());
	  int p1_long = parseInt(nextToken());
	  int p2_lat = parseInt(nextToken());
	  int p2_long = parseInt(nextToken());
	  boolean inc = (parseInt(nextToken()) != 0);
	  StreetNumberSet leftNum = makeSNS(nextToken());
	  StreetNumberSet rightNum = makeSNS(nextToken());
	  String leftZip = nextToken();
	  String rightZip = nextToken();
	  StreetClassification streetClass = StreetClassification.parse(nextToken());

	  StreetSegment seg =
	    new StreetSegment(new GeoPoint(p1_lat, p1_long),
			      new GeoPoint(p2_lat, p2_long),
			      name,
			      leftNum,
			      rightNum,
			      leftZip,
			      rightZip,
			      streetClass,
			      inc);

	  if ("WARNING".equals(command)) {
	    // for now, just ignore these
	  } else if ("KILL".equals(command)) {
	    kill.add(seg);
	  } else if ("LIMIT_ONE".equals(command)) {
	    limit_one.add(seg);
	  } else {
	    throw new KillfileReader.KillfileException("Unknown command: " + command);
	  }
	}

      } catch (IOException e) {
	throw new KillfileReader.KillfileException(e.getClass() + e.getMessage());
      }
    }

    private int parseInt(String num)
    {
      try {
	return Integer.parseInt(num);
      } catch (NumberFormatException e) {
	throw new KillfileReader.KillfileException(e.getClass() + e.getMessage() + "'" + orig_line + "'");
      }
    }

    private StreetNumberSet makeSNS(String sns)
    {
      return new StreetNumberSet(sns);
    }

    private String nextToken()

    {
      if (line == null) {
	throw new KillfileException("Ran out of tokens");
      }

      String result;
      int n = line.indexOf('$');
      if (n >= 0) {
	result = line.substring(0, n);
	line = line.substring(n+1);
      } else {
	result = line;
	line = null;
      }

      return result;
    }

  }

}
