package MapQuick3;

import java.io.IOException;
import junit.framework.TestCase;
import org.junit.Before;
import org.junit.Test;

/** Unit tests for the DrivingRouteFormatter class. */
public class DrivingRouteFormatterTest extends TestCase {

  private DrivingRouteFormatter mDirections;
  private Route mShortRoute;

  // Some constants for easy reference
  private String mName = "Penny Lane";
  private int oneUnit = 100000;

  @Before
  protected void setUp() {
    mDirections = new DrivingRouteFormatter();
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(0, 100000)));
  }

  /** Test simple directions with one distance and one origHeading. */
  @Test
  public void testShortDirections() throws IOException {
    assertEquals(new Double(90.0), new Double(mShortRoute.getStartHeading()));

    assertEquals(
        "Turn left onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 180));
    assertEquals(
        "Turn right onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 0));

    assertEquals(
        "Turn slight left onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 100));
    assertEquals(
        "Turn slight right onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 80));

    assertEquals(
        "Continue onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 99));
    assertEquals(
        "Continue onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 81));

    assertEquals(
        "Turn sharp right onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 330));
    assertEquals(
        "Turn sharp left onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 231));

    assertEquals(
        "U-turn onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 271));
    assertEquals(
        "U-turn onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 269));
  }

  /** Test turning directions with different origHeadings. */
  @Test
  public void testTurning() {

    // Left turn
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(oneUnit, 0)));
    assertEquals(
        "Turn left onto Penny Lane and go 6.9 miles.\n",
        mDirections.computeDirections(mShortRoute, 90));

    // Right turn
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(-oneUnit, 0)));
    assertEquals(
        "Turn right onto Penny Lane and go 6.9 miles.\n",
        mDirections.computeDirections(mShortRoute, 90));

    // U-turn
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(0, -oneUnit)));
    assertEquals(
        "U-turn onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 90));

    // Continue
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(0, oneUnit)));
    assertEquals(
        "Continue onto Penny Lane and go 5.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 90));

    // Slight left (15 degrees)
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(-27988, 10134)));
    assertEquals(
        "Turn slight left onto Penny Lane and go 2.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 180));

    // Slight right (15 degrees)
    mShortRoute =
        new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(-27988, -10134)));
    assertEquals(
        "Turn slight right onto Penny Lane and go 2.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 180));

    // Sharp left (165 degrees)
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(27988, 10134)));
    assertEquals(
        "Turn sharp left onto Penny Lane and go 2.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 180));

    // Sharp right (165 degrees)
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(27988, -10134)));
    assertEquals(
        "Turn sharp right onto Penny Lane and go 2.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 180));

    // U-turn (on the right side, 179.5 degree)
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(-724368, 8542)));
    assertEquals(
        "U-turn onto Penny Lane and go 50.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 0));

    // U-turn (on the left side, 179.5 degree)
    mShortRoute =
        new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(-724368, -8542)));
    assertEquals(
        "U-turn onto Penny Lane and go 50.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 0));
  }

  /**
   * Test rounding distance, especially if rounded up to 0.1 and rounded down to 0.0. Should compute
   * time before rounding.
   */
  @Test
  public void testDistance() {

    // 0.08 miles
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(0, 1566)));
    assertEquals(
        "Continue onto Penny Lane and go 0.1 miles.\n",
        mDirections.computeDirections(mShortRoute, 90));

    // 0.02 miles
    mShortRoute = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(0, 392)));
    assertEquals(
        "Continue onto Penny Lane and go 0.0 miles.\n",
        mDirections.computeDirections(mShortRoute, 90));
  }

  /** Two step route with one geo feature. */
  @Test
  public void testRepeatedSegment() {
    Route route = new Route(new GeoSegment(mName, new GeoPoint(0, 0), new GeoPoint(0, oneUnit)));
    route =
        route.addSegment(
            new GeoSegment(mName, new GeoPoint(0, oneUnit), new GeoPoint(0, oneUnit * 2)));

    assertEquals(
        "Turn left onto Penny Lane and go 10.2 miles.\n",
        mDirections.computeDirections(route, 180));
  }

  /** Long route with no repeats. */
  @Test
  public void testLongRoute() {
    Route route =
        new Route(new GeoSegment("Penny Lane", new GeoPoint(0, 0), new GeoPoint(0, oneUnit * 2)));
    route =
        route.addSegment(
            new GeoSegment(
                "Abby Road", new GeoPoint(0, oneUnit * 2), new GeoPoint(oneUnit * 2, oneUnit * 2)));

    route =
        route.addSegment(
            new GeoSegment(
                "Strawberry Fields",
                new GeoPoint(oneUnit * 2, oneUnit * 2),
                new GeoPoint(oneUnit * 4, oneUnit * 2)));

    route =
        route.addSegment(
            new GeoSegment(
                "Octopus's Garden",
                new GeoPoint(oneUnit * 4, oneUnit * 2),
                new GeoPoint(oneUnit * 4, oneUnit * 4)));

    route =
        route.addSegment(
            new GeoSegment(
                "Norwegian Wood",
                new GeoPoint(oneUnit * 4, oneUnit * 4),
                new GeoPoint(oneUnit * 10, oneUnit * 10)));

    route =
        route.addSegment(
            new GeoSegment(
                "Yellow Submarine", new GeoPoint(oneUnit * 10, oneUnit * 10), new GeoPoint(0, 0)));

    String directions =
        "Turn left onto Penny Lane and go 10.2 miles."
            + "\n"
            + "Turn left onto Abby Road and go 13.8 miles."
            + "\n"
            + "Continue onto Strawberry Fields and go 13.8 miles."
            + "\n"
            + "Turn right onto Octopus's Garden and go 10.2 miles."
            + "\n"
            + "Turn slight left onto Norwegian Wood and go 51.5 miles."
            + "\n"
            + "U-turn onto Yellow Submarine and go 85.9 miles."
            + "\n";

    assertEquals(directions, mDirections.computeDirections(route, 180));
  }

  /** Just like long route, but different makeup of geosegements. */
  @Test
  public void testRepeatedRoute() {
    Route route =
        new Route(new GeoSegment("Penny Lane", new GeoPoint(0, 0), new GeoPoint(0, oneUnit * 2)));

    route =
        route.addSegment(
            new GeoSegment(
                "Abby Road", new GeoPoint(0, oneUnit * 2), new GeoPoint(oneUnit, oneUnit * 2)));
    route =
        route.addSegment(
            new GeoSegment(
                "Abby Road",
                new GeoPoint(oneUnit, oneUnit * 2),
                new GeoPoint(oneUnit * 2, oneUnit * 2)));
    route =
        route.addSegment(
            new GeoSegment(
                "Strawberry Fields",
                new GeoPoint(oneUnit * 2, oneUnit * 2),
                new GeoPoint(oneUnit * 3, oneUnit * 2)));
    route =
        route.addSegment(
            new GeoSegment(
                "Strawberry Fields",
                new GeoPoint(oneUnit * 3, oneUnit * 2),
                new GeoPoint(oneUnit * 4, oneUnit * 2)));
    route =
        route.addSegment(
            new GeoSegment(
                "Octopus's Garden",
                new GeoPoint(oneUnit * 4, oneUnit * 2),
                new GeoPoint(oneUnit * 3, oneUnit * 3)));
    route =
        route.addSegment(
            new GeoSegment(
                "Octopus's Garden",
                new GeoPoint(oneUnit * 3, oneUnit * 3),
                new GeoPoint(oneUnit * 4, oneUnit * 3)));
    route =
        route.addSegment(
            new GeoSegment(
                "Octopus's Garden",
                new GeoPoint(oneUnit * 4, oneUnit * 3),
                new GeoPoint(oneUnit * 4, oneUnit * 4)));
    route =
        route.addSegment(
            new GeoSegment(
                "Norwegian Wood",
                new GeoPoint(oneUnit * 4, oneUnit * 4),
                new GeoPoint(oneUnit * 10, oneUnit * 10)));
    route =
        route.addSegment(
            new GeoSegment(
                "Yellow Submarine", new GeoPoint(oneUnit * 10, oneUnit * 10), new GeoPoint(0, 0)));

    String directions =
        "Turn left onto Penny Lane and go 10.2 miles."
            + "\n"
            + "Turn left onto Abby Road and go 13.8 miles."
            + "\n"
            + "Continue onto Strawberry Fields and go 13.8 miles."
            + "\n"
            + "Turn sharp right onto Octopus's Garden and go 20.6 miles."
            + "\n"
            + "Turn slight left onto Norwegian Wood and go 51.5 miles."
            + "\n"
            + "U-turn onto Yellow Submarine and go 85.9 miles."
            + "\n";

    assertEquals(directions, mDirections.computeDirections(route, 180));
  }
}
