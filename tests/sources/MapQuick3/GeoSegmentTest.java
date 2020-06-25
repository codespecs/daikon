package MapQuick3;

import static MapQuick3.TestValues.TOLERANCE;

import junit.framework.*;

/** Unit tests for the GeoSegment class. */
public class GeoSegmentTest extends TestCase {

  /** A GeoPoint object with the same data as eq2 * */
  private final GeoPoint eq1;

  /** A GeoPoint object with the same data as eq1 * */
  private final GeoPoint eq2;

  /** A GeoPoint object with different data than eq1 * */
  private final GeoPoint p2;

  /** A GeoPoint representing the center of boston * */
  private final GeoPoint p1;

  /** A GeoPoint due north of boston * */
  private final GeoPoint north;

  /** Heading, in degrees, between p1 and north * */
  private final double nHeading;

  /** Distance, in miles, between p1 and north * */
  private final double nDist;

  /** A GeoPodouble due south of boston * */
  private final GeoPoint south;

  /** Heading, in degrees, between p1 and south * */
  private final double sHeading;

  /** Distance, in miles, between p1 and south * */
  private final double sDist;

  /** A GeoPoint due east of boston * */
  private final GeoPoint east;

  /** Heading, in degrees, between p1 and east * */
  private final double eHeading;

  /** Distance, in miles, between p1 and east * */
  private final double eDist;

  /** A GeoPoint due west of boston * */
  private final GeoPoint west;

  /** Heading, in degrees, between p1 and west * */
  private final double wHeading;

  /** Distance, in miles, between p1 and west * */
  private final double wDist;

  /** A GeoPoint due north east of boston * */
  private final GeoPoint northEast;

  /** Heading, in degrees, between p1 and north east * */
  private final double neHeading;

  /** Distance, in miles, between p1 and north east * */
  private final double neDist;

  /** A GeoPoint due south west of boston * */
  private final GeoPoint southWest;

  /** Heading, in degrees, between p1 and south west * */
  private final double swHeading;

  /** Distance, in miles, between p1 and south west * */
  private final double swDist;

  /** GeoSegment that is equivalent to segEq2 * */
  private final GeoSegment segEq1;

  /** GeoSegment that is equivalent to segEq1 * */
  private final GeoSegment segEq2;

  /** GeoSegment where p1 and p2 are exact same point * */
  private final GeoSegment segSamePt;

  /** GeoSegment where p1 and p2 are equivalent points * */
  private final GeoSegment segEqPts;

  /** GeoSegment with same name as segSameName2 but different points * */
  private final GeoSegment segSameName1;

  /** GeoSegment with same name as segSameName1 but different points * */
  private final GeoSegment segSameName2;

  /** GeoSegment with a different name than segSameName1 but the same points * */
  private final GeoSegment segDiffName;

  /** GeoSegment where p1 and p2 are reversed from segRev2 * */
  private final GeoSegment segRev1;

  /** GeoSegment where p1 and p2 are reversed from segRev1 * */
  private final GeoSegment segRev2;

  /** GeoSegments pointing in N, S, E, W, NE, and SW directions * */
  private final GeoSegment segN;

  private final GeoSegment segS;
  private final GeoSegment segE;
  private final GeoSegment segW;
  private final GeoSegment segNE;
  private final GeoSegment segSW;

  public GeoSegmentTest(String name) {
    super(name);

    int eqLat = locationToInt(42, 21, 30);
    int eqLong = locationToInt(-71, 3, 37);
    eq1 = new GeoPoint(eqLat, eqLong);
    eq2 = new GeoPoint(eqLat, eqLong);
    p2 = new GeoPoint(eqLat + 50, eqLong + 50);

    // Center of boston:
    p1 = new GeoPoint(locationToInt(42, 21, 30), locationToInt(-71, 3, 37));
    // due north
    north = new GeoPoint(locationToInt(42, 22, 30), locationToInt(-71, 3, 37));
    nHeading = 0;
    nDist = 1.150406341;

    // due south
    south = new GeoPoint(locationToInt(42, 14, 19), locationToInt(-71, 3, 37));
    sHeading = 180;
    sDist = 8.263571606;

    // due east
    east = new GeoPoint(locationToInt(42, 21, 30), locationToInt(-71, 0, 17));
    eHeading = 90;
    eDist = 2.8375227;

    // due west
    west = new GeoPoint(locationToInt(42, 21, 30), locationToInt(-71, 13, 0));
    wHeading = 270;
    wDist = 7.987568175000001;

    // north east
    northEast = new GeoPoint(locationToInt(42, 22, 30), locationToInt(-71, 1, 12));
    neHeading = 60.785639340505035;
    neDist = 2.3570112129250322;

    // south west
    southWest = new GeoPoint(locationToInt(42, 14, 19), locationToInt(-71, 10, 37));
    swHeading = 215.79470947552778;
    swDist = 10.18788061670698;

    segEq1 = new GeoSegment("Eq", p1, p2);
    segEq2 = new GeoSegment("Eq", p1, p2);
    segSamePt = new GeoSegment("SamePt", p1, p1);
    segEqPts = new GeoSegment("EqPts", eq1, eq2);
    segSameName1 = new GeoSegment("SameName", p1, p2);
    segSameName2 = new GeoSegment("SameName", southWest, south);
    segDiffName = new GeoSegment("DiffName", p1, p2);
    segRev1 = new GeoSegment("Rev1", p1, p2);
    segRev2 = new GeoSegment("Rev2", p2, p1);
    segN = new GeoSegment("North", p1, north);
    segS = new GeoSegment("South", p1, south);
    segE = new GeoSegment("East", p1, east);
    segW = new GeoSegment("West", p1, west);
    segNE = new GeoSegment("NorthEast", p1, northEast);
    segSW = new GeoSegment("SouthWest", p1, southWest);
  }

  /**
   * Convert a degree, minutes, seconds latitude or longitude value to its associated integer value
   * (to millionths of a degree). Allows invalid locations to be specified for testing, but doesn't
   * allow negative minute/second values.
   */
  protected static int locationToInt(int deg, int min, int sec) {
    if (min < 0 || sec < 0) {
      throw new RuntimeException("min and sec must be positive!");
    }

    double dmin = (double) min / 60;
    double dsec = (double) sec / (60 * 60);
    if (deg >= 0) {
      return ((int) Math.round(1000000 * (deg + dmin + dsec)));
    } else {
      return ((int) Math.round(1000000 * (deg - dmin - dsec)));
    }
  }

  /** Sanity check for our locationToInt() method */
  @Test
  public void testLocationToIntMethod() {
    int lat = locationToInt(42, 21, 30);
    int lng = locationToInt(-71, 3, 37);
    if (lat != 42358333) {
      fail("42 21 30 latitude must be 42358333, is " + lat);
    } else if (lng != -71060278) {
      fail("-71 3 37 longitude must be -71060278, is " + lng);
    }
  }

  // Test method for GeoSegment(String, GeoPoint, GeoPoint)
  @Test
  public void testGeoSegment() {
    GeoSegment g = null;
    try {
      g = new GeoSegment("Test", p1, p2);
    } catch (Exception ex) {
      // Failed
      fail("Constructor failed with legal arguments");
      return;
    }

    if (g.getName() != "Test") {
      fail("Name should have been set to 'Name', it was set to '" + g.getName() + "' instead.");
    } else if (!g.getP1().equals(p1)) {
      fail("p1 not set correctly");
    } else if (!g.getP2().equals(p2)) {
      fail("p2 not set correctly");
    }
  }

  // Test method for reverse()
  @Test
  public void testReverse() {
    assertTrue(
        "segRev1.p1 should equal segRev2.reverse().p2",
        segRev1.getP1().equals(segRev2.reverse().getP1()));
    assertTrue(
        "segRev1.p2 should equal segRev2.reverse().p1",
        segRev1.getP2().equals(segRev2.reverse().getP2()));
    assertTrue(
        "segSamePt should not change under reverse()", segSamePt.reverse().equals(segSamePt));
  }

  // Test method for getName()
  @Test
  public void testGetName() {
    assertEquals(
        "getName() returned " + segN.getName() + "but North was expected.",
        segN.getName(),
        "North");
    assertEquals(
        "getName() returned " + segS.getName() + "but South was expected.",
        segS.getName(),
        "South");
    assertEquals(
        "getName() returned " + segSamePt.getName() + "but SamePt was expected.",
        segSamePt.getName(),
        "SamePt");
  }

  // Test method for getP1()
  @Test
  public void testGetP1() {
    assertTrue(
        "getP1() returned " + segN.getP1().toString() + "but " + p1.toString() + "was expected.",
        segN.getP1().equals(p1));
    assertTrue(
        "getP1() returned " + segRev2.getP1().toString() + "but " + p2.toString() + "was expected.",
        segRev2.getP1().equals(p2));
  }

  // Test method for getP2()
  @Test
  public void testGetP2() {
    assertTrue(
        "getP2() returned " + segN.getP2().toString() + "but " + north.toString() + "was expected.",
        segN.getP2().equals(north));
    assertTrue(
        "getP2() returned " + segRev1.getP2().toString() + "but " + p2.toString() + "was expected.",
        segRev1.getP2().equals(p2));
  }

  // Test method for getLength()
  @Test
  public void testGetLength() {
    assertEquals(
        "Segment length should be the same even with points reversed",
        segRev1.getLength(),
        segRev2.getLength());
    assertEquals(
        "Expected length of 0 but getLenght() returned " + segSamePt.getLength(),
        0.0,
        segSamePt.getLength(),
        TOLERANCE);
    assertEquals(
        "Expected length of 0 but getLenght() returned " + segEqPts.getLength(),
        0.0,
        segEqPts.getLength(),
        TOLERANCE);
    assertEquals(
        "Expected length of " + nDist + "but getLength() returned " + segN.getLength(),
        segN.getLength(),
        nDist,
        TOLERANCE);
    assertEquals(
        "Expected length of " + sDist + "but getLength() returned " + segS.getLength(),
        segS.getLength(),
        sDist,
        TOLERANCE);
    assertEquals(
        "Expected length of " + eDist + "but getLength() returned " + segE.getLength(),
        segE.getLength(),
        eDist,
        TOLERANCE);
    assertEquals(
        "Expected length of " + wDist + "but getLength() returned " + segW.getLength(),
        segW.getLength(),
        wDist,
        TOLERANCE);
    assertEquals(
        "Expected length of " + swDist + "but getLength() returned " + segSW.getLength(),
        segSW.getLength(),
        swDist,
        TOLERANCE);
    assertEquals(
        "Expected length of " + neDist + "but getLength() returned " + segNE.getLength(),
        segNE.getLength(),
        neDist,
        TOLERANCE);
  }

  // Test method for getHeading()
  @Test
  public void testGetHeading() {
    assertEquals(
        "Segment heading should change by 180 when the points are reversed",
        segRev1.getHeading(),
        (segRev2.getHeading() + 180) % 360);
    assertEquals(
        "Expected heading of " + nHeading + "but getHeading() returned " + segN.getHeading(),
        segN.getHeading(),
        nHeading,
        TOLERANCE);
    assertEquals(
        "Expected heading of " + sHeading + "but getHeading() returned " + segS.getHeading(),
        segS.getHeading(),
        sHeading,
        TOLERANCE);
    assertEquals(
        "Expected heading of " + eHeading + "but getHeading() returned " + segE.getHeading(),
        segE.getHeading(),
        eHeading,
        TOLERANCE);
    assertEquals(
        "Expected heading of " + wHeading + "but getHeading() returned " + segW.getHeading(),
        segW.getHeading(),
        wHeading,
        TOLERANCE);
    assertEquals(
        "Expected heading of " + swHeading + "but getHeading() returned " + segSW.getHeading(),
        segSW.getHeading(),
        swHeading,
        TOLERANCE);
    assertEquals(
        "Expected heading of " + neHeading + "but getHeading() returned " + segNE.getHeading(),
        segNE.getHeading(),
        neHeading,
        TOLERANCE);
  }

  // Test method for equals(Object)
  @Test
  public void testEquals() {
    assertTrue("These segments should be equal but equals() returned false", segEq1.equals(segEq1));
    assertTrue("These segments should be equal but equals() returned false", segEq1.equals(segEq2));
    assertFalse(
        "These segments should not be equal but equals() returned true", segN.equals(segNE));
    assertFalse(
        "These segments should not be equal but equals() returned true", segRev1.equals(segRev2));
    assertFalse(
        "These segments should not be equal but equals() returned true",
        segSameName1.equals(segSameName2));
    assertFalse(
        "These segments should not be equal but equals() returned true",
        segSameName1.equals(segDiffName));
  }

  // Test method for GeoSegment.hashCode()
  @Test
  public void testHashCode() {
    assertEquals(
        "Equivalent segments should have the same hash code", segEq1.hashCode(), segEq2.hashCode());
  }
}
