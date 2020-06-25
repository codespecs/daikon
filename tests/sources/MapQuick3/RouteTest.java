package MapQuick3;

import static MapQuick3.TestValues.TOLERANCE;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;


/**
 * Unit tests for the Route class
 */
public class RouteTest extends TestCase {

    /** A Route with zero length * */
    private final Route routeZero;

    /** A Route with first segment of zero length * */
    private final Route routeZeroStart;

    /** A Route with last segment of zero length * */
    private final Route routeZeroEnd;

    /** A Route with one segment * */
    private final Route route1;

    /** A Route with two segments * */
    private final Route route2;

    /** A Route with three segments * */
    private final Route route3;

    /** A Route that is equivalent to route3 * */
    private final Route routeEq;

    /** A Route that goes in a loop (i.e. has the same start and end point) * */
    private final Route routeLoop;

    /** Routes with multiple GeoFeatures * */
    private final Route routeFeatA, routeFeatB;

    /** GeoPoints used in constructing the Routes * */
    private final GeoPoint p1, p2, p3, p4;

    /** GeoSegments used for comparisons * */
    private final GeoSegment s1, s2, s3, s4;

    /** GeoFeatures used for comparisons with routeFeatA and routeFeatB * */
    private final GeoFeature featA1, featA2, featB1, featB2;


    public RouteTest(String name) {
        super(name);

        p1 = new GeoPoint(42356610, -71095130);
        p2 = new GeoPoint(42356430, -71095710);
        p3 = new GeoPoint(42355780, -71097670);
        p4 = new GeoPoint(42354890, -71100350);
        s1 = new GeoSegment("Test", p1, p2);
        s2 = new GeoSegment("Test", p2, p3);
        s3 = new GeoSegment("Test", p3, p4);
        s4 = new GeoSegment("Test", p4, p1);
        Route temp;


        routeZero = makeRoute("Zero", p1, p1);

        temp = makeRoute("ZeroStart", p1, p1);
        routeZeroStart = addGeoSegment(temp, "ZeroStart", p1, p2);

        temp = makeRoute("ZeroEnd", p1, p2);
        routeZeroEnd = addGeoSegment(temp, "ZeroEnd", p2, p2);

        route1 = makeRoute("Feature", p1, p2);
        route2 = addGeoSegment(route1, "Feature", p2, p3);
        route3 = addGeoSegment(route2, "Feature", p3, p4);
        routeLoop = addGeoSegment(route3, "Feature", p4, p1);

        routeEq = addGeoSegment(route2, "Feature", p3, p4);

        // Route with two GeoFeatures
        routeFeatA = addGeoSegment(route1, "Feature2", p2, p3);
        // Route with first two segments part of the same GeoFeature
        routeFeatB = addGeoSegment(route2, "Feature2", p3, p4);


        featA1 = new GeoFeature(new GeoSegment("Feature", p1, p2));
        featB1 = featA1.addSegment(new GeoSegment("Feature", p2, p3));
        featA2 = new GeoFeature(new GeoSegment("Feature2", p2, p3));
        featB2 = new GeoFeature(new GeoSegment("Feature2", p3, p4));

    }

    /**
     * This is a helper function to make it easier to construct a Route. It takes a name and
     * two GeoPoints and creates a Route consisting of a single GeoSegment described by the
     * arguments.
     */
    protected static Route makeRoute(String name, GeoPoint p1, GeoPoint p2) {
        return new Route(new GeoSegment(name, p1, p2));
    }

    /**
     * This is a helper function that makes it easier to add to a Route. It creates a
     * GeoSegment defined by the last three arguments and adds it to the end of the Route.
     */
    protected static Route addGeoSegment(Route r, String name, GeoPoint p1, GeoPoint p2) {
        return r.addSegment(new GeoSegment(name, p1, p2));
    }


    // JUnit calls setUp() before each test__ method is run
    protected void setUp() {

    }

//  Test for the Constructor

    public void testRoute() {
        Route r = null;
        GeoSegment gs = new GeoSegment("Test", p1, p2);
        try {
            r = makeRoute("Test", p1, p1);
        } catch (Exception ex) {
            // Failed
            fail("Constructor failed when geoSegment had zero length");
            return;
        }

        try {
            r = new Route(gs);
        } catch (Exception ex) {
            // Failed
            fail("Constructor failed with valid arguments");
            return;
        }

        if (!r.getStart().equals(p1)) {
            fail("start not set correctly");
        } else if (!r.getEnd().equals(p2)) {
            fail("end not set correctly");
        } else {
            assertEquals("StartHeading not set properly", r.getStartHeading(), gs.getHeading(),
                    TOLERANCE);
            assertEquals("EndHeading not set properly", r.getEndHeading(), gs.getHeading(),
                    TOLERANCE);
        }

    }


    // Test for getStart()
    public void testGetStart() {
        assertTrue(routeZero.getStart().equals(p1));
        assertTrue(routeZeroStart.getStart().equals(p1));
        assertTrue(routeZeroEnd.getStart().equals(p1));
        assertTrue(route1.getStart().equals(p1));
        assertTrue(route3.getStart().equals(p1));
        assertTrue(routeLoop.getStart().equals(p1));
    }

    // Test fot getEnd()
    public void testGetEnd() {
        assertTrue(routeZero.getEnd().equals(p1));
        assertTrue(routeZeroStart.getEnd().equals(p2));
        assertTrue(routeZeroEnd.getEnd().equals(p2));
        assertTrue(route1.getEnd().equals(p2));
        assertTrue(route3.getEnd().equals(p4));
        assertTrue(routeLoop.getEnd().equals(p1));
    }

    // Test for getStartHeading()
    public void testGetStartHeading() {
        assertEquals(routeZeroEnd.getStartHeading(), s1.getHeading(), TOLERANCE);
        assertEquals(route1.getStartHeading(), s1.getHeading(), TOLERANCE);
        assertEquals(route2.getStartHeading(), s1.getHeading(), TOLERANCE);
        assertEquals(route3.getStartHeading(), s1.getHeading(), TOLERANCE);
        assertEquals(routeLoop.getStartHeading(), s1.getHeading(), TOLERANCE);
    }

    // Test for getEndHeading()
    public void testGetEndHeading() {
        assertEquals(routeZeroStart.getEndHeading(), s1.getHeading(), TOLERANCE);
        assertEquals(route1.getEndHeading(), s1.getHeading(), TOLERANCE);
        assertEquals(route2.getEndHeading(), s2.getHeading(), TOLERANCE);
        assertEquals(route3.getEndHeading(), s3.getHeading(), TOLERANCE);
        assertEquals(routeLoop.getEndHeading(), s4.getHeading(), TOLERANCE);
    }

    // Test for getLength()
    public void testGetLength() {
        assertEquals(routeZero.getLength(), 0, TOLERANCE);
        assertEquals(routeZeroStart.getLength(), s1.getLength(), TOLERANCE);
        assertEquals(routeZeroEnd.getLength(), s1.getLength(), TOLERANCE);
        assertEquals(route1.getLength(), s1.getLength(), TOLERANCE);
        assertEquals(route2.getLength(), s1.getLength()+s2.getLength(), TOLERANCE);
        assertEquals(route3.getLength(), s1.getLength()+s2.getLength()+s3.getLength(), TOLERANCE);
        assertEquals(routeLoop.getLength(), s1.getLength()+s2.getLength()+s3.getLength()+s4.getLength(), TOLERANCE);
    }

    // Test for addSegment()
    public void testAddSegment() {
        Route r = null;
        r = addGeoSegment(route1, "Feature", p2, p2); // add zero length segment
        checkAdd(r, s1.getLength(), p2);
        r = addGeoSegment(route1, "Feature", p2, p3); // add 1 segment
        checkAdd(r, s1.getLength()+s2.getLength(), p3);
        r = addGeoSegment(r, "Feature", p3, p4); // add another segment
        checkAdd(r, s1.getLength()+s2.getLength()+s3.getLength(), p4);
        r = addGeoSegment(r, "Feature", p4, p1); // added segment makes a loop
        checkAdd(r, s1.getLength()+s2.getLength()+s3.getLength()+s4.getLength(), p1);
    }

    // Helper function for testAddSegment. Checks the that the length and endpoint are correct.
    protected void checkAdd(Route r, double length, GeoPoint end) {

        assertEquals("Length should be " + length + " but it was " + r.getLength(), length,
                r.getLength(), TOLERANCE);
        assertTrue("The endpoint was not set correctly", end.equals(r.getEnd()));

    }

    // Test for getGeoSegments()
    public void testGetGeoSegments() {
        List<GeoSegment> segments;

        segments = new ArrayList<GeoSegment>();
        segments.add(new GeoSegment("Feature", p1, p2));
        checkSegments(segments.iterator(), route1.getGeoSegments());
        segments.add(new GeoSegment("Feature", p2, p3));
        checkSegments(segments.iterator(), route2.getGeoSegments());
        segments.add(new GeoSegment("Feature", p3, p4));
        checkSegments(segments.iterator(), route3.getGeoSegments());
        segments.add(new GeoSegment("Feature", p4, p1));
        checkSegments(segments.iterator(), routeLoop.getGeoSegments());
    }

    // Helper function for getGeoSegments. It will compare the two iterators and make sure they
    // iterate over the exact same items.

    protected void checkSegments (Iterator<GeoSegment> expected, Iterator<GeoSegment> actual) {

        while (expected.hasNext() && actual.hasNext()) {
            assertTrue("Iterator returned wrong GeoSegment", expected.next().equals(actual.next()));
        }
        if (expected.hasNext() && !actual.hasNext())
            fail("Not enough elements in iterator");
        else if (!expected.hasNext() && actual.hasNext())
            fail("Too many elements in iterator");
    }

    // Test for getGeoFeatures()
    public void testGetGeoFeatures() {
        List<GeoFeature> features = new ArrayList<GeoFeature>();

        features.add(featA1);
        checkFeatures(features.iterator(), route1.getGeoFeatures());
        features.add(featA2);
        checkFeatures(features.iterator(), routeFeatA.getGeoFeatures());

        features.clear();
        features.add(featB1);
        checkFeatures(features.iterator(), route2.getGeoFeatures());
        features.add(featB2);
        checkFeatures(features.iterator(), routeFeatB.getGeoFeatures());
    }

    // Helper function for getGeoFeatures. It will compare the two iterators and make sure they
    // iterate over the exact same items.

    protected void checkFeatures (Iterator<GeoFeature> expected, Iterator<GeoFeature> actual) {

        while (expected.hasNext() && actual.hasNext()) {
            assertTrue("Iterator returned wrong GeoSegment", expected.next().equals(actual.next()));
        }
        if (expected.hasNext() && !actual.hasNext())
            fail("Not enough elements in iterator");
        else if (!expected.hasNext() && actual.hasNext())
            fail("Too many elements in iterator");
    }


    // Test for equals()
    public void testEquals() {
        assertTrue("A Route should be equal to itself",route1.equals(route1));
        assertTrue(route3.equals(routeEq));
        assertFalse(route1.equals(route2));
        assertFalse(route1.equals(route3));
        assertFalse(route2.equals(route3));
        assertFalse(route3.equals(routeLoop));
        assertFalse(route1.equals(routeLoop));
    }

    // Test for hashCode()
    public void testHashCode() {
        assertEquals(route1.hashCode(), route1.hashCode());
        assertEquals(route3.hashCode(), routeEq.hashCode());
    }


}
