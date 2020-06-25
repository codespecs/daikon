package MapQuick;

import MapQuick2.*;
import junit.framework.*;
import org.junit.Test;

public class StreetSegmentTest extends TestCase {

  public StreetSegmentTest(String name) {
    super(name);
  }

  @Test
  public void testFractionDestBasic() {
    GeoPoint gpDowntown = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr = new StreetNumberSet("1,3,7");
    StreetNumberSet snsl = new StreetNumberSet("");

    StreetSegment ss =
        new StreetSegment(
            gpDowntown,
            gpWest,
            "Western Ave.",
            snsr,
            snsl,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    assertEquals("First address on a sample", 0.25, ss.fractionDist(1), 0.001);
    assertEquals("Second address on a sample", 0.5, ss.fractionDist(3), 0.001);
    assertEquals("Third address on a sample", 0.75, ss.fractionDist(7), 0.001);
  }

  @Test
  public void testFractionDist() {
    GeoPoint gpDowntown = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr = new StreetNumberSet("200-298");
    StreetNumberSet snsl = new StreetNumberSet("201-299");

    StreetSegment ss =
        new StreetSegment(
            gpDowntown,
            gpWest,
            "Western Ave.",
            snsr,
            snsl,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    assertEquals("First address on a long block", 0.0392, ss.fractionDist(200), 0.001);
    assertEquals("Last address on a long block", 1.9608, ss.fractionDist(298), 0.001);
    assertEquals("First address on a long block", 0.0392, ss.fractionDist(201), 0.001);
    assertEquals("Last address on a long block", 1.9608, ss.fractionDist(299), 0.001);
  }

  @Test
  public void testEquals1() {
    GeoPoint gpDowntown = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr = new StreetNumberSet("1,3,7");
    StreetNumberSet snsl = new StreetNumberSet("");

    StreetSegment ss1 =
        new StreetSegment(
            gpDowntown,
            gpWest,
            "Western Ave.",
            snsr,
            snsl,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    StreetSegment ss2 =
        new StreetSegment(
            gpDowntown,
            gpWest,
            "Western Ave.",
            snsr,
            snsl,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    assertEquals("StreetSegment not equal to itself.", ss1, ss1);
    assertEquals("Two identical StreetSegments not equal.", ss1, ss2);
  }

  @Test
  public void testEquals2() {
    GeoPoint gpDowntown1 = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest1 = new GeoPoint(42358333, -71079857);
    GeoPoint gpDowntown2 = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest2 = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr1 = new StreetNumberSet("1,3,7");
    StreetNumberSet snsl1 = new StreetNumberSet("");
    StreetNumberSet snsr2 = new StreetNumberSet("1,3,7");
    StreetNumberSet snsl2 = new StreetNumberSet("");

    StreetSegment ss1 =
        new StreetSegment(
            gpDowntown1,
            gpWest1,
            "Western Ave.",
            snsr1,
            snsl1,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    StreetSegment ss2 =
        new StreetSegment(
            gpDowntown2,
            gpWest2,
            "Western Ave.",
            snsr2,
            snsl2,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    assertEquals("Two StreetSegments built with equal components not equal.", ss1, ss2);
  }

  @Test
  public void testEquals3() {
    GeoPoint gpDowntown = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr = new StreetNumberSet("1,3,7");
    StreetNumberSet snsl = new StreetNumberSet("");

    StreetSegment ss =
        new StreetSegment(
            gpDowntown,
            gpWest,
            "Western Ave.",
            snsr,
            snsl,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    assertTrue("StreetSegment equal to null.", !ss.equals(null));
    assertTrue("StreetSegment equal to String object.", !ss.equals("Hello"));
  }

  @Test
  public void testHashCode() {
    GeoPoint gpDowntown1 = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest1 = new GeoPoint(42358333, -71079857);
    GeoPoint gpDowntown2 = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest2 = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr1 = new StreetNumberSet("1,3,5,8");
    StreetNumberSet snsl1 = new StreetNumberSet("");
    StreetNumberSet snsr2 = new StreetNumberSet("1-5,8");
    StreetNumberSet snsl2 = new StreetNumberSet("");

    StreetSegment ss1 =
        new StreetSegment(
            gpDowntown1,
            gpWest1,
            "Western Ave.",
            snsr1,
            snsl1,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    StreetSegment ss2 =
        new StreetSegment(
            gpDowntown2,
            gpWest2,
            "Western Ave.",
            snsr2,
            snsl2,
            "02139",
            "02138",
            StreetClassification.LOCAL_ROAD,
            true);
    assertEquals(
        "hashCodes of '1,3,5,8' and '1-5,8' are not equal.", ss1.hashCode(), ss2.hashCode());
  }
  // Tell JUnit what order to run the tests in
  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTest(new StreetSegmentTest("testFractionDistBasic"));
    suite.addTest(new StreetSegmentTest("testFractionDist"));
    suite.addTest(new StreetSegmentTest("testEquals1"));
    suite.addTest(new StreetSegmentTest("testEquals2"));
    suite.addTest(new StreetSegmentTest("testEquals3"));
    return suite;
  }
}
