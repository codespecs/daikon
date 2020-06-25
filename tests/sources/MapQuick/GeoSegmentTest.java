package MapQuick;

import MapQuick2.*;


import junit.framework.*;

public class GeoSegmentTest extends TestCase {

  private static final double tolerance = GeoPointTest.tolerance;

  private GeoPoint gpDowntown = null;
  private GeoPoint gpWest     = null;
  private GeoPoint gpEast     = null;
  private GeoPoint gpNorth    = null;

  private GeoSegment gsEast   = null;
  private GeoSegment gsWest   = null;
  private GeoSegment gsNorth  = null;
  private GeoSegment gsEast2  = null;
  private GeoSegment gsWest2  = null;
  private GeoSegment gsDiag   = null;
  // private GeoSegment gsZero   = null;

  public GeoSegmentTest(String name) {
    super(name);
  }

  // JUnit calls setUp() before each test__ method is run
  protected void setUp() {
    //                         north       east
    // +1 mile                   14488      19579
    gpDowntown = new GeoPoint(42358333, -71060278);
    gpWest     = new GeoPoint(42358333, -71079857);
    gpEast     = new GeoPoint(42358333, -71040699);
    gpNorth    = new GeoPoint(42372821, -71060278);

    gsEast = new GeoSegment("East",  gpDowntown, gpEast);
    gsWest = new GeoSegment("West",  gpDowntown, gpWest);
    gsNorth = new GeoSegment("North", gpDowntown, gpNorth);
    gsEast2 = new GeoSegment("East", gpWest, gpDowntown);
    gsWest2 = new GeoSegment("West", gpWest, gpDowntown);
    gsDiag = new GeoSegment("NE", gpWest, gpNorth);
    // gsZero = new GeoSegment("Zero", gpDowntown, gpDowntown);
  }



  public void testEquals() {
    assertEquals("Self equality", gsNorth, gsNorth);
    assertEquals("Equal to copy", gsNorth,
                 new GeoSegment("North", gpDowntown, gpNorth));
    assertTrue("totally different objects are equal.", !gsNorth.equals(gsEast));
    assertTrue("same points, different name are equal.", !gsEast2.equals(gsWest2));
    assertTrue("same name, different points are equal.", !gsEast.equals(gsEast2));

  }

  public void testReverse() {
    assertTrue("Reversed segment is not equal same segment reversed.",
               gsEast.reverse().equals(gsEast.reverse()));
    assertTrue("Twice reversed segment is not same as initial.",
               gsEast.reverse().reverse().equals(gsEast));
    assertTrue("New reversed item is not equal to its reversal.",
               gsWest.equals(gsWest2.reverse()));
    assertTrue("New reversed item is not equal to its reversal.",
               gsWest.reverse().equals(gsWest2));
    assertTrue("Segment equal to its reversal.",
               !gsEast.reverse().equals(gsEast));
    assertTrue("Segment reversed twice is equal to its reversal.",
               !gsEast.reverse().reverse().equals(gsEast.reverse()));
    assertTrue("Reversed segment reversed equals reversed.",
               !gsWest.reverse().equals(gsWest2.reverse()));
    // assertTrue("Reversed zero segment doesn't equal itself.",
    //        gsZero.reverse().equals(gsZero));
  }

  public void testName() {
    assertEquals("name() doesn't work.",
		 "East", gsEast.name());
  }

  public void testP1() {
    assertEquals(gpDowntown, gsEast.p1());
  }

  public void testP2() {
    assertEquals(gpDowntown, gsEast2.p2());
  }

  public void testLength() {
    assertEquals("East 1 mile", 1.0, gsEast.length(), tolerance);
    assertEquals("West 1 mile", 1.0, gsWest.length(), tolerance);
    assertEquals("North 1 mile", 1.0, gsNorth.length(), tolerance);
    assertEquals("1.414 miles", 1.414, gsDiag.length(), tolerance);
    // assertEquals("Zero length", 0.0, gsZero.length(), tolerance);
  }

  public void testHeading() {
    assertEquals("East should be 90", 90.0, gsEast.heading(), tolerance);
    assertEquals("West should be 270", 270.0, gsWest.heading(), tolerance);
    double nh = gsNorth.heading();
    assertTrue("North heading (" + nh + ") is less than zero.", !(nh < 0.0));
    assertTrue("North heading (" + nh + ") is greater or equal to 360.", !(nh >= 360.0));
    if (nh > tolerance) {
      // we know nh is in [0..360); maybe it's just under 360, which is okay too
      double delta = Math.abs(360.0 - nh);
      if (delta > tolerance)
        fail("North heading expected: 0 or 359.999 but got " + nh);
    }
    assertEquals("South heading should be 180",
                 180.0, gsNorth.reverse().heading(), tolerance);
  }

  public void testEquals2() {
    // make segment components which are equal by value, but which are
    // not the same object
    String north = "Nor";
    north += "th";
    GeoPoint gpDowntown2 = new GeoPoint(42358333, -71060278);
    GeoPoint gpNorth2    = new GeoPoint(42372821, -71060278);
    GeoSegment gsNorth2 = new GeoSegment(north, gpDowntown2, gpNorth2);

    assertTrue("Segment equality should use value equality, not reference equality",
               gsNorth.equals(gsNorth2));

    assertTrue("equals(non-GeoSegment) should be false",
               !gsNorth2.equals("aString"));

    assertTrue("equals(null) should be false",
               !gsNorth2.equals(null));
  }

  public void testHashCode() {
    GeoPoint gpDowntown2 = new GeoPoint(42358333, -71060278);
    GeoPoint gpNorth2    = new GeoPoint(42372821, -71060278);
    GeoSegment gsNorth2 = new GeoSegment("North", gpDowntown2, gpNorth2);

    assertTrue(".equals() objects must have the same .hashCode()",
               gsNorth.hashCode() == gsNorth2.hashCode());
  }

  // Tell JUnit what order to run the tests in
  public static Test suite()
  {
    TestSuite suite = new TestSuite();
    suite.addTest(new GeoSegmentTest("testEquals"));
    suite.addTest(new GeoSegmentTest("testEquals2"));
    suite.addTest(new GeoSegmentTest("testHashCode"));
    suite.addTest(new GeoSegmentTest("testReverse"));
    suite.addTest(new GeoSegmentTest("testName"));
    suite.addTest(new GeoSegmentTest("testP1"));
    suite.addTest(new GeoSegmentTest("testP2"));
    suite.addTest(new GeoSegmentTest("testLength"));
    suite.addTest(new GeoSegmentTest("testHeading"));
    return suite;
  }

}
