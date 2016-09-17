package MapQuick3;

import junit.framework.*;


/**
 * SpecificationTests is a simple TestSuite that includes and runs all
 * the tests in {@link GeoPointTest}, {@link GeoSegmentTest}, {@link
 * GeoFeatureTest}, {@link RouteTest}, {@link
 * WalkingRouteFormatterTest}, and {@link DrivingRouteFormatterTest}.
 */
public final class SpecificationTests extends TestSuite {

    public SpecificationTests() {
        this("Problem Set 2 Specification Tests");
    }

    public SpecificationTests(String name) {
        super(name);

        addTestSuite(GeoPointTest.class);
        addTestSuite(GeoSegmentTest.class);
        addTestSuite(GeoFeatureTest.class);
        addTestSuite(RouteTest.class);
        addTestSuite(WalkingRouteFormatterTest.class);
        addTestSuite(DrivingRouteFormatterTest.class);
    }

    public static Test suite() {
        return new SpecificationTests();
    }
}
