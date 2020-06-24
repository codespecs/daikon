package MapQuick3;

import static MapQuick3.TestValues.TOLERANCE;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import junit.framework.TestCase;

/** Unit tests for the GeoFeature class */
public class GeoFeatureTest extends TestCase {

  /** A GeoFeature with zero length * */
  private final GeoFeature featZero;

  /** A GeoFeature with first segment of zero length * */
  private final GeoFeature featZeroStart;

  /** A GeoFeature with last segment of zero length * */
  private final GeoFeature featZeroEnd;

  /** A GeoFeature with one segment * */
  private final GeoFeature feat1;

  /** A GeoFeature with two segments * */
  private final GeoFeature feat2;

  /** A GeoFeature with three segments * */
  private final GeoFeature feat3;

  /** A GeoFeature with the same name as feat1 but with different GeoSegments * */
  private final GeoFeature featSameName;

  /** A GeoFeature with the same GeoSegments as feat1 but with a different name * */
  private final GeoFeature featDiffName;

  /** A GeoFeature that is equivalent to feat3 * */
  private final GeoFeature featEq;

  /** A GeoFeature that goes in a loop (i.e. has the same start and end point) * */
  private final GeoFeature featLoop;

  /** GeoPoints used in constructing the GeoFeatures * */
  private final GeoPoint p1, p2, p3, p4;

  /** GeoSegments used for comparisons * */
  private final GeoSegment s1, s2, s3, s4;

  public GeoFeatureTest(String name) {
    super(name);

    p1 = new GeoPoint(42356610, -71095130);
    p2 = new GeoPoint(42356430, -71095710);
    p3 = new GeoPoint(42355780, -71097670);
    p4 = new GeoPoint(42354890, -71100350);
    s1 = new GeoSegment("Test", p1, p2);
    s2 = new GeoSegment("Test", p2, p3);
    s3 = new GeoSegment("Test", p3, p4);
    s4 = new GeoSegment("Test", p4, p1);
    GeoFeature temp;

    featZero = makeGeoFeature("Zero", p1, p1);

    temp = makeGeoFeature("ZeroStart", p1, p1);
    featZeroStart = addGeoSegment(temp, "ZeroStart", p1, p2);

    temp = makeGeoFeature("ZeroEnd", p1, p2);
    featZeroEnd = addGeoSegment(temp, "ZeroEnd", p2, p2);

    feat1 = makeGeoFeature("Feature", p1, p2);
    feat2 = addGeoSegment(feat1, "Feature", p2, p3);
    feat3 = addGeoSegment(feat2, "Feature", p3, p4);
    featLoop = addGeoSegment(feat3, "Feature", p4, p1);

    featSameName = makeGeoFeature("Feature", p3, p4);
    featDiffName = makeGeoFeature("Diff", p1, p2);

    featEq = addGeoSegment(feat2, "Feature", p3, p4);
  }

  /**
   * This is a helper function to make it easier to construct a GeoFeature. It takes a name and two
   * GeoPoints and creates a GeoFeature consisting of a single GeoSegment described by the
   * arguments.
   */
  protected static GeoFeature makeGeoFeature(String name, GeoPoint p1, GeoPoint p2) {
    return new GeoFeature(new GeoSegment(name, p1, p2));
  }

  /**
   * This is a helper function that makes it easier to add to a GeoFeature. It creates a GeoSegment
   * defined by the last three arguments and adds it to the end of the GeoFeature.
   */
  protected static GeoFeature addGeoSegment(GeoFeature gf, String name, GeoPoint p1, GeoPoint p2) {
    return gf.addSegment(new GeoSegment(name, p1, p2));
  }

  // JUnit calls setUp() before each test__ method is run
  protected void setUp() {}

  // Test for the Constructor

  @Test
  public void testGeoFeature() {
    GeoFeature gf = null;
    GeoSegment gs = new GeoSegment("Test", p1, p2);
    try {
      gf = makeGeoFeature("Test", p1, p1);
    } catch (Exception ex) {
      // Failed
      fail("Constructor failed when geoSegment had zero length");
      return;
    }

    try {
      gf = new GeoFeature(gs);
    } catch (Exception ex) {
      // Failed
      fail("Constructor failed with valid arguments");
      return;
    }

    if (gf.getName() != "Test") {
      fail("Name should have been set to 'Name', it was set to '" + gf.getName() + "' instead.");
    } else if (!gf.getStart().equals(p1)) {
      fail("start not set correctly");
    } else if (!gf.getEnd().equals(p2)) {
      fail("end not set correctly");
    } else {
      assertEquals(
          "StartHeading not set properly", gf.getStartHeading(), gs.getHeading(), TOLERANCE);
      assertEquals("EndHeading not set properly", gf.getEndHeading(), gs.getHeading(), TOLERANCE);
    }
  }

  // Test for getName()
  @Test
  public void testGetName() {
    assertEquals(featZero.getName(), "Zero");
    assertEquals(feat1.getName(), "Feature");
    assertEquals(feat3.getName(), "Feature");
  }

  // Test for getStart()
  @Test
  public void testGetStart() {
    assertTrue(featZero.getStart().equals(p1));
    assertTrue(featZeroStart.getStart().equals(p1));
    assertTrue(featZeroEnd.getStart().equals(p1));
    assertTrue(feat1.getStart().equals(p1));
    assertTrue(feat3.getStart().equals(p1));
    assertTrue(featLoop.getStart().equals(p1));
  }

  // Test fot getEnd()
  @Test
  public void testGetEnd() {
    assertTrue(featZero.getEnd().equals(p1));
    assertTrue(featZeroStart.getEnd().equals(p2));
    assertTrue(featZeroEnd.getEnd().equals(p2));
    assertTrue(feat1.getEnd().equals(p2));
    assertTrue(feat3.getEnd().equals(p4));
    assertTrue(featLoop.getEnd().equals(p1));
  }

  // Test for getStartHeading()
  @Test
  public void testGetStartHeading() {
    assertEquals(featZeroEnd.getStartHeading(), s1.getHeading(), TOLERANCE);
    assertEquals(feat1.getStartHeading(), s1.getHeading(), TOLERANCE);
    assertEquals(feat2.getStartHeading(), s1.getHeading(), TOLERANCE);
    assertEquals(feat3.getStartHeading(), s1.getHeading(), TOLERANCE);
    assertEquals(featLoop.getStartHeading(), s1.getHeading(), TOLERANCE);
  }

  // Test for getEndHeading()
  @Test
  public void testGetEndHeading() {
    assertEquals(featZeroStart.getEndHeading(), s1.getHeading(), TOLERANCE);
    assertEquals(feat1.getEndHeading(), s1.getHeading(), TOLERANCE);
    assertEquals(feat2.getEndHeading(), s2.getHeading(), TOLERANCE);
    assertEquals(feat3.getEndHeading(), s3.getHeading(), TOLERANCE);
    assertEquals(featLoop.getEndHeading(), s4.getHeading(), TOLERANCE);
  }

  // Test for getLength()
  @Test
  public void testGetLength() {
    assertEquals(featZero.getLength(), 0, TOLERANCE);
    assertEquals(featZeroStart.getLength(), s1.getLength(), TOLERANCE);
    assertEquals(featZeroEnd.getLength(), s1.getLength(), TOLERANCE);
    assertEquals(feat1.getLength(), s1.getLength(), TOLERANCE);
    assertEquals(feat2.getLength(), s1.getLength() + s2.getLength(), TOLERANCE);
    assertEquals(feat3.getLength(), s1.getLength() + s2.getLength() + s3.getLength(), TOLERANCE);
    assertEquals(
        featLoop.getLength(),
        s1.getLength() + s2.getLength() + s3.getLength() + s4.getLength(),
        TOLERANCE);
  }

  // Test for addSegment()
  @Test
  public void testAddSegment() {
    GeoFeature gf = null;
    gf = addGeoSegment(feat1, "Feature", p2, p2); // add zero length segment
    checkAdd(gf, s1.getLength(), p2);
    gf = addGeoSegment(feat1, "Feature", p2, p3); // add 1 segment
    checkAdd(gf, s1.getLength() + s2.getLength(), p3);
    gf = addGeoSegment(gf, "Feature", p3, p4); // add another segment
    checkAdd(gf, s1.getLength() + s2.getLength() + s3.getLength(), p4);
    gf = addGeoSegment(gf, "Feature", p4, p1); // added segment makes a loop
    checkAdd(gf, s1.getLength() + s2.getLength() + s3.getLength() + s4.getLength(), p1);
  }

  // Helper function for testAddSegment. Checks the that the length and endpoint are correct.
  protected void checkAdd(GeoFeature gf, double length, GeoPoint end) {

    assertEquals(
        "Length should be " + length + " but it was " + gf.getLength(),
        length,
        gf.getLength(),
        TOLERANCE);
    assertTrue("The endpoint was not set correctly", end.equals(gf.getEnd()));
  }

  // Test for getGeoSegments()
  @Test
  public void testGetGeoSegments() {
    List<GeoSegment> segments;

    segments = new ArrayList<GeoSegment>();
    segments.add(new GeoSegment("Feature", p1, p2));
    checkSegments(segments.iterator(), feat1.getGeoSegments());
    segments.add(new GeoSegment("Feature", p2, p3));
    checkSegments(segments.iterator(), feat2.getGeoSegments());
    segments.add(new GeoSegment("Feature", p3, p4));
    checkSegments(segments.iterator(), feat3.getGeoSegments());
    segments.add(new GeoSegment("Feature", p4, p1));
    checkSegments(segments.iterator(), featLoop.getGeoSegments());
  }

  // Helper function for getGeoSegments. It will compare the two iterators and make sure they
  // iterate over the exact same items.

  protected void checkSegments(Iterator<GeoSegment> expected, Iterator<GeoSegment> actual) {

    while (expected.hasNext() && actual.hasNext()) {
      assertTrue("Iterator returned wrong GeoSegment", expected.next().equals(actual.next()));
    }
    if (expected.hasNext() && !actual.hasNext()) fail("Not enough elements in iterator");
    else if (!expected.hasNext() && actual.hasNext()) fail("Too many elements in iterator");
  }

  // Test for equals()
  @Test
  public void testEquals() {
    assertTrue("A GeoFeature should be equal to itself", feat1.equals(feat1));
    assertTrue(feat3.equals(featEq));
    assertFalse(feat1.equals(feat2));
    assertFalse(feat1.equals(feat3));
    assertFalse(feat2.equals(feat3));
    assertFalse(feat3.equals(featLoop));
    assertFalse(feat1.equals(featLoop));
    assertFalse(feat1.equals(featSameName));
    assertFalse(feat1.equals(featDiffName));
  }

  // Test for hashCode()
  @Test
  public void testHashCode() {
    assertEquals(feat1.hashCode(), feat1.hashCode());
    assertEquals(feat3.hashCode(), featEq.hashCode());
  }
}
