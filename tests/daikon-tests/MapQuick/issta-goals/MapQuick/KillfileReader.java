package MapQuick;

import MapQuick2.*;


import java.io.*;
import java.util.*;

/**
 * Abstraction to read and return filters based on killfiles
 */
public class KillfileReader
{
  /*@ invariant MapQuick.KillfileReader.debug == false; */
  /*@ invariant MapQuick.KillfileReader.killfile_filter != null; */
  /*@ spec_public */ private static boolean debug = false;
  /*@ requires s != null; */
  private static void debugln(String s) { if (debug) { System.err.println("KFR: " + s); } }

  /*@ requires killfile != null; */
  /*@ ensures \result != null; */
  /**
   * @return a filter which passes segments not listed in the given killfile
   */
  public static StreetSegmentFilter fromFile(File killfile)
  {
    debugln("Reading " + killfile);
    return new KillfileFilter(killfile);
  }

  //@ requires dbdir.isDirectory // outside of Daikon's grammar
  /*@ requires dbdir != null; */
  /*@ ensures \result != null; */
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

    StreetSegmentFilter result = fromFile(files[0]);
    for (int i=1; i<files.length; i++) {
      StreetSegmentFilter another = fromFile(files[i]);
      result = new CompositeStreetSegmentFilter(result, another);
    }

    return result;
  }

  // filter which gives us *killfile.txt files
  /*@ spec_public */ private static final FilenameFilter killfile_filter = new FNFilter();
  private static class FNFilter
    implements FilenameFilter
  {
    /*@ also_requires d != null; */
    /*@ also_requires name != null; */
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

    /*@ invariant this.one != null; */
    /*@ invariant this.two != null; */
    /*@ requires one != null; */
    /*@ requires two != null; */
    /*@ ensures one == this.one; */
    /*@ ensures two == this.two; */
    public CompositeStreetSegmentFilter(StreetSegmentFilter one, StreetSegmentFilter two)
    {
      this.one = one;
      this.two = two;
    }

    /*@ spec_public */ private final StreetSegmentFilter one;
    /*@ spec_public */ private final StreetSegmentFilter two;

    /*@ also_requires seg != null; */
    /**@ also_requires this != null; */
    /**@ also_requires this.one != null; */
    /**@ also_requires this.two != null; */
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

    /*@ also_requires seg != null; */
    /*# also_ensures this.line == \old(this.orig_line); */ // not sure: should have been suppressed?
    /**@ also_ensures (\result == false)  ==>  (this != null); */
    /*@ also_ensures (\result == false)  ==>  (this.kill != null); */
    /*@ also_ensures (\result == false)  ==>  (this.limit_one != null); */
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

    //@ invariant kill.elementType == \type(Object) // dumb
    /*@ spec_public */ private final Set kill = new HashSet();
    //@ invariant limit_one.elementType == \type(Object) // dumb
    /*@ spec_public */ private final Set limit_one = new HashSet();
    {
      //@ set kill.elementType = \type(Object) // dumb
      //@ set limit_one.elementType = \type(Object) // dumb
    }


    // not a real field, but want scope in c-tor and helper
    /*@ spec_public */ private String line;
    /*@ spec_public */ private String orig_line;

    /*@ requires killfile != null; */
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
	    //@ assume false // engineering
            throw new KillfileReader.KillfileException("Unknown command: " + command);
          }
        }

      } catch (IOException e) {
	//@ assume false // engineering
        throw new KillfileReader.KillfileException(e.getClass() + e.getMessage());
      }
    }

    /*@ requires num != null; */
    /*@ requires this.line != null; */
    /*@ requires this.orig_line != null; */
    private int parseInt(String num)
    {
      try {
        return Integer.parseInt(num);
      } catch (NumberFormatException e) {
	//@ assume false // engineering
        throw new KillfileReader.KillfileException(e.getClass() + e.getMessage() + "'" + orig_line + "'");
      }
    }

    /*@ requires sns != null; */
    /*@ requires this.line != null; */
    /*@ requires this.orig_line != null; */
    /*@ ensures \result != null; */
    private StreetNumberSet makeSNS(String sns)
    {
      return new StreetNumberSet(sns);
    }

    /*@ requires this.line != null; */
    /*@ requires this.orig_line != null; */
    /*@ modifies this.line; */
    /*@ ensures \result != null; */
    //@ ensures this.line != null // incompleteness: modular analysis
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
	//@ assume false // incompleteness: modular analysis
        result = line;
        line = null;
      }

      return result;
    }

  }

}

