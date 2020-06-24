package MapQuick;

import MapQuick2.*;
import junit.framework.*;

public class GeoPointTest extends TestCase {

  public static final double tolerance = 0.01;

  private GeoPoint gpDowntown = null;
  private GeoPoint gpWest = null;
  private GeoPoint gpEast = null;
  private GeoPoint gpSouthEast = null;
  private GeoPoint gpNorth = null;
  private GeoPoint gpSouth = null;
  private GeoPoint gpMit = null;

  public GeoPointTest(String name) {
    super(name);
  }

  protected void setUp() {

    gpDowntown = new GeoPoint(42358333, -71060278);
    gpWest = new GeoPoint(42358333, -71079857);
    gpEast = new GeoPoint(42358333, -71040699);
    gpNorth = new GeoPoint(42372821, -71060278);
    gpSouthEast = new GeoPoint(42343845, -71040699);
    ;
    gpSouth = new GeoPoint(42343845, -71060278);
  }

  @Test
  public void testImmutability() {
    GeoPoint gpReceiver = new GeoPoint(42358333, -71060278);
    GeoPoint gpArgument = new GeoPoint(42343845, -71060278);
    gpReceiver.distanceTo(gpArgument);
    assertTrue("distanceTo mutates receiver.", gpReceiver.equals(gpDowntown));
    assertTrue("distanceTo mutates argument.", gpArgument.equals(gpSouth));
    gpReceiver.hashCode();
    assertTrue("distanceTo mutates receiver.", gpReceiver.equals(gpDowntown));
    gpReceiver.headingTo(gpArgument);
    assertTrue("headingTo mutates receiver.", gpReceiver.equals(gpDowntown));
    assertTrue("headingTo mutates argument.", gpArgument.equals(gpSouth));
    gpReceiver.toString();
    assertTrue("toString mutates receiver.", gpReceiver.equals(gpDowntown));
  }

  @Test
  public void testEquals() {
    assertTrue("instance not equal to itself.", gpNorth.equals(gpNorth));
    assertTrue("instance not equal to copy.", gpNorth.equals(new GeoPoint(42372821, -71060278)));
    assertTrue("totally different objects are equal.", !gpNorth.equals(gpEast));
  }

  @Test
  public void testEquals2() {
    assertTrue("equals returns true for null.", !gpNorth.equals(null));
    assertTrue("equals returns true for a String.", !gpNorth.equals("foo"));
  }

  @Test
  public void testDistanceTo() {
    assertEquals("From North to Downtown 1 mile", 1.0, gpNorth.distanceTo(gpDowntown), tolerance);
    assertEquals(
        "From North to Downtown equal to Downtown to North",
        gpDowntown.distanceTo(gpNorth),
        gpNorth.distanceTo(gpDowntown),
        tolerance);
    assertEquals("From North to North 0 miles", 0.0, gpNorth.distanceTo(gpNorth), tolerance);
  }

  @Test
  public void testHeadingTo() {
    assertEquals(
        "Downtown headingTo East should be 90", 90.0, gpDowntown.headingTo(gpEast), tolerance);
    assertEquals(
        "Downtown headingTo West should be 270", 270.0, gpDowntown.headingTo(gpWest), tolerance);

    // assertEquals("Downtown headingTo North should be 0",0.0, gpDowntown.headingTo(gpNorth),
    // tolerance);
    double h = gpDowntown.headingTo(gpNorth);
    assertTrue(
        "Downtown headingTo North should be 0",
        ((h - 0.0) <= tolerance) || ((360.0 - h) <= tolerance));

    assertEquals(
        "Downtown headingTo South should be 180", 180.0, gpDowntown.headingTo(gpSouth), tolerance);
    assertEquals("West headingTo North should be 45", 45.0, gpWest.headingTo(gpNorth), tolerance);
    assertEquals("West headingTo South should be 135", 135.0, gpWest.headingTo(gpSouth), tolerance);
    assertEquals("East headingTo North should be 315", 315.0, gpEast.headingTo(gpNorth), tolerance);
    assertEquals("East headingTo South should be 225", 225.0, gpEast.headingTo(gpSouth), tolerance);
  }

  @Test
  public void testHashCode() {
    GeoPoint gpDowntown2 = new GeoPoint(42358333, -71060278);

    assertTrue(
        ".equals() objects must have the same .hashCode()",
        gpDowntown.hashCode() == gpDowntown2.hashCode());
  }

  // Tell JUnit what order to run the tests in
  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTest(new GeoPointTest("testEquals"));
    suite.addTest(new GeoPointTest("testEquals2"));
    suite.addTest(new GeoPointTest("testHashCode"));
    suite.addTest(new GeoPointTest("testDistanceTo"));
    suite.addTest(new GeoPointTest("testHeadingTo"));
    suite.addTest(new GeoPointTest("testImmutability"));
    return suite;
  }
}
