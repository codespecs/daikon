package MapQuick3;

import junit.framework.TestCase;

import java.io.IOException;


/**
 * Unit tests for the WalkingRouteFormatter class.
 */
public class WalkingRouteFormatterTest extends TestCase {

    private WalkingRouteFormatter mDirections;
    private Route mShortRoute;

    // Some constants for easy reference
    private String mName = "Penny Lane";
    private int oneUnit = 100000;

    // JUnit calls setUp() before each test__ method is run
    protected void setUp() {
        mDirections = new WalkingRouteFormatter();
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,100000)));
    }


    /**
     * Test simple directions with one distance and one origHeading.
     */
    public void testShortDirections() throws IOException {
        assertEquals(new Double(90.0),
                     new Double(mShortRoute.getStartHeading()));

        assertEquals("Turn left onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 180));
        assertEquals("Turn right onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 0));

        assertEquals("Turn slight left onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 100));
        assertEquals("Turn slight right onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 80));

        assertEquals("Continue onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 99));
        assertEquals("Continue onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 81));

        assertEquals("Turn left onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 181));
        assertEquals("Turn right onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 359));

        assertEquals("Turn sharp right onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 300));
        assertEquals("Turn sharp left onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 231));

        assertEquals("U-turn onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 269));
        assertEquals("U-turn onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 271));
    }


    /**
     * Test turning directions, with different origHeadings.
     */
    public void testTurning() {

        // Left turn
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(oneUnit,0)));
        assertEquals("Turn left onto Penny Lane and walk for 138 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 90));

        // Right turn
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(-oneUnit,0)));
        assertEquals("Turn right onto Penny Lane and walk for 138 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 90));

        // U-turn
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,-oneUnit)));
        assertEquals("U-turn onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 90));

        // Continue
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,oneUnit)));
        assertEquals("Continue onto Penny Lane and walk for 102 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 90));

        // Slight left (15 degrees)
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(-27988,10134)));
        assertEquals("Turn slight left onto Penny Lane and walk for 40 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 180));

        // Slight right (15 degrees)
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(-27988,-10134)));
        assertEquals("Turn slight right onto Penny Lane and walk for 40 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 180));

        // Sharp left (165 degrees)
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(27988,10134)));
        assertEquals("Turn sharp left onto Penny Lane and walk for 40 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 180));

        // Sharp right (165 degrees)
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(27988,-10134)));
        assertEquals("Turn sharp right onto Penny Lane and walk for 40 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 180));

        // U-turn (on the left side, 179.5 degree)
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(-724368,8542)));
        assertEquals("U-turn onto Penny Lane and walk for 1000 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 0));

        // U-turn (on the right side, 179.5 degree)
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(-724368,-8542)));
        assertEquals("U-turn onto Penny Lane and walk for 1000 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 0));
    }


    /**
     * Test rounding distance, specifically if it rounds up to 0.1 and
     * rounds down to 0.0.  Should compute time before rounding.
     */
    public void testDistance() {
        // 0.08 miles
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,1566)));
        assertEquals("Continue onto Penny Lane and walk for 2 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 90));

        // 0.02 miles
        mShortRoute = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,392)));
        assertEquals("Continue onto Penny Lane and walk for 0 minutes.\n",
                     mDirections.computeDirections(mShortRoute, 90));
    }


    /**
     * Two step route with one geo feature.
     */
    public void testRepeatedSegment() {
        Route route = new Route(new GeoSegment(mName,
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,oneUnit)));
        route = route.addSegment(new GeoSegment(mName,
                                                new GeoPoint(0,oneUnit),
                                                new GeoPoint(0,oneUnit*2)));

        assertEquals("Turn left onto Penny Lane and walk for 204 minutes.\n",
                     mDirections.computeDirections(route, 180));
    }


    /**
     * Long route with no repeats.
     */
    public void testLongRoute() {
        Route route = new Route(new GeoSegment("Penny Lane",
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Abby Road",
                                                new GeoPoint(0,oneUnit*2),
                                                new GeoPoint(oneUnit*2,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Strawberry Fields",
                                                new GeoPoint(oneUnit*2,oneUnit*2),
                                                new GeoPoint(oneUnit*4,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Octopus's Garden",
                                                new GeoPoint(oneUnit*4,oneUnit*2),
                                                new GeoPoint(oneUnit*4,oneUnit*4)));

        route = route.addSegment(new GeoSegment("Norwegian Wood",
                                                new GeoPoint(oneUnit*4,oneUnit*4),
                                                new GeoPoint(oneUnit*10,oneUnit*10)));

        route = route.addSegment(new GeoSegment("Yellow Submarine",
                                                new GeoPoint(oneUnit*10,oneUnit*10),
                                                new GeoPoint(0,0)));
        String directions =
            "Turn left onto Penny Lane and walk for 204 minutes." + "\n" +
            "Turn left onto Abby Road and walk for 276 minutes." + "\n" +
            "Continue onto Strawberry Fields and walk for 276 minutes." + "\n" +
            "Turn right onto Octopus's Garden and walk for 204 minutes." + "\n" +
            "Turn slight left onto Norwegian Wood and walk for 1030 minutes." + "\n" +
            "U-turn onto Yellow Submarine and walk for 1717 minutes." + "\n"
            ;
        assertEquals(directions,
                     mDirections.computeDirections(route, 180));
    }


    /**
     * Just like long route, but different makeup of geosegements.
     */
    public void testRepeatedRoute() {
        Route route = new Route(new GeoSegment("Penny Lane",
                                               new GeoPoint(0,0),
                                               new GeoPoint(0,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Abby Road",
                                                new GeoPoint(0,oneUnit*2),
                                                new GeoPoint(oneUnit,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Abby Road",
                                                new GeoPoint(oneUnit,oneUnit*2),
                                                new GeoPoint(oneUnit*2,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Strawberry Fields",
                                                new GeoPoint(oneUnit*2,oneUnit*2),
                                                new GeoPoint(oneUnit*3,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Strawberry Fields",
                                                new GeoPoint(oneUnit*3,oneUnit*2),
                                                new GeoPoint(oneUnit*4,oneUnit*2)));
        route = route.addSegment(new GeoSegment("Octopus's Garden",
                                                new GeoPoint(oneUnit*4,oneUnit*2),
                                                new GeoPoint(oneUnit*3,oneUnit*3)));
        route = route.addSegment(new GeoSegment("Octopus's Garden",
                                                new GeoPoint(oneUnit*3,oneUnit*3),
                                                new GeoPoint(oneUnit*4,oneUnit*3)));
        route = route.addSegment(new GeoSegment("Octopus's Garden",
                                                new GeoPoint(oneUnit*4,oneUnit*3),
                                                new GeoPoint(oneUnit*4,oneUnit*4)));
        route = route.addSegment(new GeoSegment("Norwegian Wood",
                                                new GeoPoint(oneUnit*4,oneUnit*4),
                                                new GeoPoint(oneUnit*10,oneUnit*10)));
        route = route.addSegment(new GeoSegment("Yellow Submarine",
                                                new GeoPoint(oneUnit*10,oneUnit*10),
                                                new GeoPoint(0,0)));

        String directions =
            "Turn left onto Penny Lane and walk for 204 minutes." + "\n" +
            "Turn left onto Abby Road and walk for 276 minutes." + "\n" +
            "Continue onto Strawberry Fields and walk for 276 minutes." + "\n" +
            "Turn sharp right onto Octopus's Garden and walk for 412 minutes." + "\n" +
            "Turn slight left onto Norwegian Wood and walk for 1030 minutes." + "\n" +
            "U-turn onto Yellow Submarine and walk for 1717 minutes." + "\n"
            ;

        assertEquals(directions,
                     mDirections.computeDirections(route, 180));

    }
}
