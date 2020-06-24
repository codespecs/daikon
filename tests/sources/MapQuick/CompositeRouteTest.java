package MapQuick;

import MapQuick2.*;
import java.util.*;
import junit.framework.*;

// TODO: check to see what happens if you have a path that loops back
// upon itself; either immediately or if it ever reaches the same
// GeoPoint twice.

public class CompositeRouteTest extends TestCase {

  private static final double tolerance = GeoPointTest.tolerance;

  private GeoPoint gpDowntown = null;
  private GeoPoint gpWest = null;
  private GeoPoint gpWest2 = null;
  private GeoPoint gpSouth = null;
  private GeoPoint gpEast = null;

  private GeoSegment gsZero = null;
  private GeoSegment gsWest = null;
  private GeoSegment gsWest2 = null;
  private GeoSegment gsSouth = null;
  private GeoSegment gsNE = null;

  private ElementaryRoute erWest = null;
  private ElementaryRoute erSouth = null;
  private CompositeRoute crZero = null;
  private CompositeRoute crWest = null;

  public CompositeRouteTest(String name) {
    super(name);
  }

  protected void setUp() {

    gpDowntown = new GeoPoint(42358333, -71060278);
    gpWest = new GeoPoint(42358333, -71079857);
    gpWest2 = new GeoPoint(42358333, -71099436);
    gpSouth = new GeoPoint(42343845, -71060278);
    gpEast = new GeoPoint(42358333, -71040699);

    gsWest = new GeoSegment("West", gpDowntown, gpWest);
    gsWest2 = new GeoSegment("West", gpWest, gpWest2);
    gsSouth = new GeoSegment("South", gpWest2, gpSouth);
    gsNE = new GeoSegment("NorthEast", gpSouth, gpEast);

    erWest = new ElementaryRoute(gsWest);
    erSouth = new ElementaryRoute(gsSouth);
    crWest = new CompositeRoute(gsWest);
  }

  @Test
  public void testImmutability() {
    CompositeRoute rReceiver = new CompositeRoute(new GeoSegment("West", gpDowntown, gpWest));
    CompositeRoute rArgument = new CompositeRoute(new GeoSegment("West", gpWest, gpWest2));
    GeoSegment gsArgument = new GeoSegment("West", gpWest, gpWest2);
    rReceiver.addSegment(gsArgument);
    assertTrue("addSegment mutates receiver.", rReceiver.equals(crWest));
    assertTrue("addSegment mutates argument.", gsArgument.equals(gsWest2));
    rReceiver.directions(0.0);
    assertTrue("directions mutates receiver.", rReceiver.equals(crWest));
    rReceiver.hashCode();
    assertTrue("hashCode mutates receiver.", rReceiver.equals(crWest));
    rReceiver.elementaryRoutes();
    assertTrue("elementaryRoutes mutates receiver.", rReceiver.equals(crWest));
    rReceiver.end();
    assertTrue("end mutates receiver.", rReceiver.equals(crWest));
    rReceiver.endHeading();
    assertTrue("endHeading mutates receiver.", rReceiver.equals(crWest));
    rReceiver.length();
    assertTrue("length mutates receiver.", rReceiver.equals(crWest));
    rReceiver.start();
    assertTrue("start mutates receiver.", rReceiver.equals(crWest));
    rReceiver.startHeading();
    assertTrue("startHeading mutates receiver.", rReceiver.equals(crWest));
  }

  public void testStart() {
    assertTrue(
        "the start point of rWest is not the same than the start point of the segment gsWest it was created with",
        crWest.start().equals(gpDowntown));
  }

  @Test
  public void testEnd() {
    assertTrue(
        "the end point of rWest is not the same than the start point of the segment gsWest it was created with",
        crWest.end().equals(gpWest));
  }

  @Test
  public void testStartHeading() {
    assertEquals(
        "the start heading of rWest is not the same the heading of the segment gWest it was created with",
        crWest.startHeading(),
        gsWest.heading(),
        tolerance);
  }

  @Test
  public void testEndHeading() {
    assertEquals(
        "the end heading of rWest is not the same the heading of the segment gWest it was created with",
        crWest.endHeading(),
        gsWest.heading(),
        tolerance);
  }

  @Test
  public void testLength() {
    assertEquals(
        "the length of rWest is not the same the length of the segment gWest it was created with",
        crWest.length(),
        gsWest.length(),
        tolerance);
  }

  @Test
  public void testEquals() {
    CompositeRoute crWest2 = new CompositeRoute(gsWest);
    assertTrue(
        "rWest and rWest2 are created both from gsWest, but they are not equal",
        crWest.equals(crWest2));
  }

  @Test
  public void testAddSegment() {

    CompositeRoute cr = crWest.addSegment(gsWest2);

    // the specs
    assertEquals(
        "the length of crWest.addSegment(gsWest2) is not the sum of the length of crWest + gsWest2",
        cr.length(),
        (crWest.length() + gsWest2.length()),
        tolerance);
    assertTrue(
        "the end point crWest.addSegment(gsWest2) is not the same end point than gsWest2",
        cr.end().equals(gpWest2));
    assertTrue(
        "the start point crWest.addSegment(gsWest2) is not the same start point than rWest",
        cr.start().equals(gpDowntown));
    assertEquals(
        "the end heading of crWest.addSegment(gsWest2) is not the same the heading of gsWest2",
        cr.endHeading(),
        gsWest2.heading(),
        tolerance);
    assertEquals(
        "the start heading of crWest.addSegment(gsWest2) is not the same the start heading of rWest",
        cr.startHeading(),
        gsWest.heading(),
        tolerance);
    assertEquals(
        "the array of elementaryRoute[] of crWest.addSegment(gsWest2) should only contain 1 element",
        cr.elementaryRoutes().length,
        1,
        tolerance);

    ElementaryRoute er = erWest.addSegment(gsWest2);
    assertTrue(
        "the elementaryRoute in the ElementaryRoute[] of crWest.addSegment(gsWest2) is not the right object.",
        cr.elementaryRoutes()[0].equals(er));
  }

  @Test
  public void testAddSegmentWithEquals() {

    CompositeRoute cr = crWest.addSegment(gsWest2);
    CompositeRoute cr2 = cr.addSegment(gsSouth);
    assertEquals(
        "the length of cr2:<gsWest,gsWest2,gsSouth> is not the sum of the length of cr0:<gsWest,gsWest2> + cr1:<gsSouth>",
        cr2.length(),
        (cr.length() + gsSouth.length()),
        tolerance);
    assertTrue(
        "the end point of cr2:<gsWest,gsWest2,gsSouth> is not the same end point than gsSouth",
        cr2.end().equals(gpSouth));
    assertTrue(
        "the start point of cr2:<gsWest,gsWest2,gsSouth> is not the same start point than gsSouth",
        cr2.start().equals(gpDowntown));
    assertEquals(
        "the start heading of cr2:<gsWest,gsWest2,gsSouth>  is not the same the start heading of gsWest",
        cr2.startHeading(),
        cr.startHeading(),
        tolerance);
    assertEquals(
        "the end heading of cr2:<gsWest,gsWest2,gsSouth>  is not the same the end heading of gsWest",
        cr2.endHeading(),
        gsSouth.heading(),
        tolerance);
    if (cr2.elementaryRoutes().length != 2)
      fail(
          "there should be two ElementaryRoute in this.element, there are "
              + cr2.elementaryRoutes().length
              + " instead.");
    else
      assertTrue(
          "the second elementaryRoutes in the ElementaryRoute[] is not the right object.",
          cr2.elementaryRoutes()[1].equals(erSouth));
  }

  @Test
  public void testAddSegmentSameNameButJagged() {

    GeoPoint center = new GeoPoint(42358333, -71060278);
    GeoPoint farSouth = new GeoPoint(42200000, -71060278);
    GeoPoint slightWest = new GeoPoint(42358333, -71079857);

    GeoSegment step1 = new GeoSegment("Jagged", center, farSouth);
    GeoSegment step2 = new GeoSegment("Jagged", farSouth, slightWest);

    CompositeRoute cr = new CompositeRoute(step1);
    cr = cr.addSegment(step2);

    assertEquals(
        "Length of same-name-but-jagged CompositeRoute should be the distance travelled",
        center.distanceTo(farSouth) + farSouth.distanceTo(slightWest),
        cr.length(),
        tolerance);
    assertEquals(
        "Start heading of same-name-but-jagged CompositeRoute should be the original heading",
        center.headingTo(farSouth),
        cr.startHeading(),
        tolerance);
    assertEquals(
        "End heading of same-name-but-jagged CompositeRoute should be the final heading",
        farSouth.headingTo(slightWest),
        cr.endHeading(),
        tolerance);

    assertAlmostEquals(
        "Directions for jagged composite route of with the same name all along",
        "U-turn onto Jagged and go 21.9 miles.\n",
        cr.directions(0.0));
  }

  @Test
  public void testEquals2() {
    GeoSegment gsDowntownSouth = new GeoSegment("West", gpDowntown, gpSouth);
    GeoSegment gsWestSouth = new GeoSegment("West", gpWest, gpSouth);
    CompositeRoute rDW2 = new CompositeRoute(gsDowntownSouth);
    CompositeRoute rDWW2 = crWest.addSegment(gsWestSouth);

    // this test relies on a correct addSegment()
    assertTrue(
        "rDWW2 is created from (gpDowntown -> gpWest) + (gpWest -> gpSouth), it should not be equal to rDW2 created from (gpDowntown -> gpSouth)",
        !rDW2.equals(rDWW2));
  }

  @Test
  public void testDirections() {

    CompositeRoute r0 = crWest.addSegment(gsWest2);
    CompositeRoute r1 = r0.addSegment(gsSouth);
    CompositeRoute r2 = r1.addSegment(gsNE);

    assertEquals(
        "directions",
        new String(
                "Turn left onto West and go 2.0 miles."
                    + "\n"
                    + "Turn sharp left onto South and go 2.2 miles."
                    + "\n"
                    + "Turn left onto NorthEast and go 1.4 miles.")
            .trim(),
        r2.directions(0).trim());
  }

  private static String stripWhiteSpace(String s) {
    char[] inBuff = s.toCharArray();
    char[] outBuff = new char[inBuff.length];
    int l = 0;

    for (int i = 0; i < inBuff.length; i++) {
      if (!Character.isWhitespace(inBuff[i])) {
        outBuff[l] = inBuff[i];
        l++;
      }
    }
    return new String(outBuff, 0, l);
  }

  private static String removeString(String source, String pattern) {
    int t = source.indexOf(pattern);
    if (t < 0) return source;
    String firstHalf = source.substring(0, t);
    String secondHalf = source.substring(t + 2);
    return firstHalf + secondHalf;
  }

  private static boolean almostEquals(String a, String b) {
    a = stripWhiteSpace(a);
    b = stripWhiteSpace(b);
    a = removeString(a, "to");
    b = removeString(b, "to");
    a = a.toLowerCase();
    b = b.toLowerCase();
    return a.equals(b);
  }

  private static void assertAlmostEquals(String message, String a, String b) {
    if (!almostEquals(a, b)) fail(message + ": " + "Expected: " + a + ", " + "Got: " + b);
  }

  @Test
  public void testApproxDirections() {

    CompositeRoute r0 = crWest.addSegment(gsWest2);
    CompositeRoute r1 = r0.addSegment(gsSouth);
    CompositeRoute r2 = r1.addSegment(gsNE);

    assertAlmostEquals(
        "directions",
        new String(
            "Turn left onto West and go 2.0 miles."
                + "\n"
                + "Turn sharp left onto South and go 2.2 miles."
                + "\n"
                + "Turn left onto NorthEast and go 1.4 miles.\n"),
        r2.directions(0));
  }

  @Test
  public void testElementaryRoutes() {
    ElementaryRoute[] era = crWest.elementaryRoutes();
    if (era == null)
      fail(
          "the array ElementaryRoute[] of a CompositeRoute with one GeoSegment is null, it should have one element");
    if (era.length == 0)
      fail(
          "the array ElementaryRoute[] of a CompositeRoute with one GeoSegment is empty, it should have one element");
    if (era.length > 1)
      fail(
          "the array of ElementaryRoute[] of a CompositeRoute with one GeoSegment should have a size of 1, instead it is "
              + era.length);
    assertTrue(
        "the array of ElementaryRoutes of the CompositeRoute with one GeoSegment doesn't contain the right elementaryRoute",
        era[0].equals(erWest));

    ElementaryRoute er = erWest.addSegment(gsWest2);
    CompositeRoute cr = crWest.addSegment(gsWest2);
    ElementaryRoute[] era2 = cr.elementaryRoutes();
    if (era2 == null)
      fail(
          "the array ElementaryRoute[] of a CompositeRoute with two GeoSegments of the same name is null, it should have one element");
    else if (era2.length == 0)
      fail(
          "the array ElementaryRoute[] of a CompositeRoute with two GeoSegments of the same name is empty, it should have one element");
    else if (era2.length != 1)
      fail("the array of ElementaryRoute should have a size of 1, instead it is " + era2.length);
    else
      assertTrue(
          "the array of ElementaryRoute[] of a CompositeRoute with two GeoSegments of the same name doesn't contain the right ElementaryRoute",
          era2[0].equals(er));

    CompositeRoute cr2 = cr.addSegment(gsSouth);
    ElementaryRoute[] era3 = cr2.elementaryRoutes();
    if (era3 == null)
      fail(
          "the array ElementaryRoute[] of a CompositeRoute with two GeoSegments of different name is null, it should have one element");
    else if (era3.length == 0)
      fail(
          "the array ElementaryRoute[] of a CompositeRoute with two GeoSegments of different name is empty, it should have one element");
    else if (era3.length != 2)
      fail(
          "the array of ElementaryRoute[] of a CompositeRoute with two GeoSegments of different name should have a size of 2, instead it is "
              + era3.length);
    else {
      assertTrue(
          "the array of ElementaryRoute[] of a CompositeRoute with two GeoSegments of different name doesn't contain the right first ElementaryRoute",
          era3[0].equals(er));
      assertTrue(
          "the array of ElementaryRoute[] of a CompositeRoute with two GeoSegments of different name doesn't contain the right second ElementaryRoute",
          era3[1].equals(erSouth));
    }
  }

  // Tell JUnit what order to run the tests in
  public static Test suite() {
    TestSuite suite = new TestSuite();

    suite.addTest(new GeoSegmentTest("testStart"));
    suite.addTest(new GeoSegmentTest("testEnd"));
    suite.addTest(new GeoSegmentTest("testStartHeading"));
    suite.addTest(new GeoSegmentTest("testEndHeading"));
    suite.addTest(new GeoSegmentTest("testLength"));
    suite.addTest(new GeoSegmentTest("testEquals"));
    suite.addTest(new GeoSegmentTest("testaddSegment"));
    suite.addTest(new GeoSegmentTest("testaddSegmentWithEquals"));
    suite.addTest(new GeoSegmentTest("testAddSegmentSameNameButJagged"));
    suite.addTest(new GeoSegmentTest("testEquals2"));
    suite.addTest(new GeoSegmentTest("testElementaryRoutes"));
    suite.addTest(new GeoSegmentTest("testDirections"));
    suite.addTest(new GeoSegmentTest("testApproxDirections"));
    suite.addTest(new GeoSegmentTest("testImmutability"));

    return suite;
  }
}
