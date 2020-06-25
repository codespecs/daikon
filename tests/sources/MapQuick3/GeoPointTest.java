package MapQuick3;

import static MapQuick3.TestValues.TOLERANCE;

import junit.framework.*;

/** Unit tests for the GeoPoint class. */
public class GeoPointTest extends TestCase {

  /** A GeoPoint object with the same data as eq2 */
  private final GeoPoint eq1;

  /** A GeoPoint object with the same data as eq1 */
  private final GeoPoint eq2;

  /** A GeoPoint object with different data than eq1 */
  private final GeoPoint diff;

  /** A GeoPoint representing the center of boston */
  private final GeoPoint p1;

  /** A GeoPoint due north of boston */
  private final GeoPoint north;

  /** Heading, in degrees, between p1 and north */
  private final double nHeading;

  /** Distance, in miles, between p1 and north */
  private final double nDist;

  /** A GeoPodouble due south of boston */
  private final GeoPoint south;

  /** Heading, in degrees, between p1 and south */
  private final double sHeading;

  /** Distance, in miles, between p1 and south */
  private final double sDist;

  /** A GeoPoint due east of boston */
  private final GeoPoint east;

  /** Heading, in degrees, between p1 and east */
  private final double eHeading;

  /** Distance, in miles, between p1 and east */
  private final double eDist;

  /** A GeoPoint due west of boston */
  private final GeoPoint west;

  /** Heading, in degrees, between p1 and west */
  private final double wHeading;

  /** Distance, in miles, between p1 and west */
  private final double wDist;

  /** A GeoPoint due north east of boston */
  private final GeoPoint northEast;

  /** Heading, in degrees, between p1 and north east */
  private final double neHeading;

  /** Distance, in miles, between p1 and north east */
  private final double neDist;

  /** A GeoPoint due south west of boston */
  private final GeoPoint southWest;

  /** Heading, in degrees, between p1 and south west */
  private final double swHeading;

  /** Distance, in miles, between p1 and south west */
  private final double swDist;

  public GeoPointTest(String name) {
    super(name);

    int eqLat = locationToInt(42, 21, 30);
    int eqLong = locationToInt(-71, 3, 37);
    eq1 = new GeoPoint(eqLat, eqLong);
    eq2 = new GeoPoint(eqLat, eqLong);
    diff = new GeoPoint(eqLat + 50, eqLong + 50);

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

  ////////////////////////////////////////////////////
  // TEST LEGAL/ILLEGAL VALUES IN GeoPoint CREATION //
  ////////////////////////////////////////////////////

  /**
   * Tests that GeoPoints can be created with some legal latitude and longitude values that are in
   * the greater boston area, and that the values it's provided are returned unchanged.
   */
  @Test
  public void testLegalValues1() {
    int valLat = locationToInt(42, 21, 30);
    int valLong = locationToInt(-71, 3, 37);
    checkLegal(valLat, valLong);
  }

  /**
   * Tests that GeoPoints can be created with some legal latitude and longitude values that are in
   * the greater boston area, and that the values it's provided are returned unchanged.
   */
  @Test
  public void testLegalValues2() {
    int valLat = locationToInt(42, 21, 60);
    int valLong = locationToInt(-71, 14, 7);
    checkLegal(valLat, valLong);
  }

  /**
   * Helper method for asserting that a legal latitude/longitude pair can be used to create a
   * GeoPoint and that those exact same values are then returned.
   */
  protected void checkLegal(int lat, int lng) {
    GeoPoint g = null;
    try {
      g = new GeoPoint(lat, lng);
    } catch (Exception ex) {
      // Failed
      fail("Didn't allow legal (latitude,longitude) of (" + lat + ", " + lng + ")");
      return;
    }

    if (g.getLatitude() != lat) {
      fail(
          "Latitude should have been set to "
              + lat
              + ", was set to "
              + g.getLatitude()
              + " instead.");

    } else if (g.getLongitude() != lng) {
      fail(
          "Longitude should have been set to "
              + lng
              + ", was set to "
              + g.getLongitude()
              + " instead.");
    }
  }

  //////////////////////////////
  // TEST GeoPoint.distanceTo //
  //////////////////////////////

  /** Test that distance 0 is handled correctly: same object */
  @Test
  public void testDistance01() {
    assertEquals(
        "Distance between a GeoPoint and itself must be 0", 0.0, p1.distanceTo(p1), TOLERANCE);
  }

  /** Test that distance 0 is handled correctly: equal objects */
  @Test
  public void testDistance02() {
    assertEquals(
        "Distance between two equal GeoPoints must be 0", 0.0, eq1.distanceTo(eq2), TOLERANCE);
  }

  /** Test that points due north are handled correctly */
  @Test
  public void testDistanceDueNorth() {
    checkDist(p1, north, nDist);
  }

  /** Test that points due south are handled correctly */
  @Test
  public void testDistanceDueSouth() {
    checkDist(p1, south, sDist);
  }

  /** Test that points due east are handled correctly */
  @Test
  public void testDistanceDueEast() {
    checkDist(p1, east, eDist);
  }

  /** Test that points due west are handled correctly */
  @Test
  public void testDistanceDueWest() {
    checkDist(p1, west, wDist);
  }

  /** Test that points north east are handled correctly */
  @Test
  public void testDistanceDueNorthEast() {
    checkDist(p1, northEast, neDist);
  }

  /** Test that points south west are handled correctly */
  @Test
  public void testDistanceDueSouthWest() {
    checkDist(p1, southWest, swDist);
  }

  /**
   * Helper method that asserts that the distance between p1 and p2 is dist AND that the distance
   * between p2 and p1 is also dist.
   */
  protected void checkDist(GeoPoint p1, GeoPoint p2, double dist) {
    assertEquals(
        "Distance between these two points is " + dist + ", not " + p1.distanceTo(p2),
        dist,
        p1.distanceTo(p2),
        TOLERANCE);
    assertEquals(
        "Reversing between these two points is " + dist + ", not " + p2.distanceTo(p1),
        dist,
        p2.distanceTo(p1),
        TOLERANCE);
  }

  /////////////////////////////
  // TEST GeoPoint.headingTo //
  /////////////////////////////

  /** Test that points due north are handled correctly */
  @Test
  public void testHeadingDueNorth() {
    checkHeading(p1, north, nHeading);
  }

  /** Test that points due south are handled correctly */
  @Test
  public void testHeadingDueSouth() {
    checkHeading(p1, south, sHeading);
  }

  /** Test that points due east are handled correctly */
  @Test
  public void testHeadingDueEast() {
    checkHeading(p1, east, eHeading);
  }

  /** Test that points due west are handled correctly */
  @Test
  public void testHeadingDueWest() {
    checkHeading(p1, west, wHeading);
  }

  /** Test that points north east are handled correctly */
  @Test
  public void testHeadingNorthEast() {
    checkHeading(p1, northEast, neHeading);
  }

  /** Test that points south west are handled correctly */
  @Test
  public void testHeadingSouthWest() {
    checkHeading(p1, southWest, swHeading);
  }

  /**
   * Helper method that asserts that the heading between p1 and p2 is heading AND that the distance
   * between p2 and p1 is heading + 180.
   */
  protected void checkHeading(GeoPoint p1, GeoPoint p2, double heading) {
    assertEquals(
        "Heading between these two points is " + heading + ", not " + p1.headingTo(p2),
        heading,
        p1.headingTo(p2),
        TOLERANCE);
    assertEquals(
        "Reversing between these two points is " + (heading + 180) + ", not " + p2.headingTo(p1),
        heading,
        ((p2.headingTo(p1) + 180) % 360),
        TOLERANCE);
  }

  //////////////////////////
  // TEST GeoPoint.equals //
  //////////////////////////

  /** Test positive case: same object */
  @Test
  public void testEquals1() {
    assertTrue("A GeoPoint must be equal() to itself", eq1.equals(eq1));
  }

  /** Test positive case: equal objects */
  @Test
  public void testEquals2() {
    assertTrue("Two GeoPoints with the same data must be equal()", eq1.equals(eq2));
  }

  /** Test that null values are handled correctly */
  @Test
  public void testNotEqualsNull() {
    try {
      if (!eq1.equals(null)) {
        // Good, said not equal
        return;
      }
    } catch (Exception ex) {
      // Bad, should just return false
      fail("Null not handled: equals(null) throws an exception " + "instead of returning false.");
      return;
    }

    fail("Null not handled: equals(null) returns true!");
  }

  /** Test that two GeoPoints with different data do not register as equal. */
  @Test
  public void testNotEqualsDiff() {
    assertFalse("Two GeoPoints with different data must not be equal()", eq1.equals(diff));
  }

  /**
   * Test that two GeoPoints with "swapped" latitude and longitude values (i.e., they both sum to
   * the same value) do not register as equal.
   */
  @Test
  public void testNotEqualsSwapped() {
    int lat = locationToInt(42, 21, 30);
    int lng = locationToInt(-71, 3, 37);
    GeoPoint e1 = new GeoPoint(lat, lng);
    GeoPoint e2 = new GeoPoint(lng, lat);
    assertFalse("Two GeoPoints with swapped data must not be equal()", e1.equals(e2));
  }

  ////////////////////////////
  // TEST GeoPoint.hashCode //
  ////////////////////////////

  /** Test positive case: same object */
  @Test
  public void testHashCode1() {
    assertEquals("A GeoPoint's hashCode must remain constant", eq1.hashCode(), eq1.hashCode());
  }

  /** Test positive case: equal objects */
  @Test
  public void testHashCode2() {
    assertEquals(
        "GeoPoints with same data must have the same hashCode", eq1.hashCode(), eq2.hashCode());
  }
}
