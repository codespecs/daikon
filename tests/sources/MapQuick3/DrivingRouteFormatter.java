package MapQuick3;

import java.text.DecimalFormat;


/**
 * <p>
 * The DrivingRouteFormatter class creates a textual description of
 * directions for traversing a route that are suitable for a driver of
 * a vehicle.
 * </p>
 *
 * <p>
 * Calling <tt>computeDirections</tt> should produce directions in the
 * following form:
 * </p>
 *
 * <p>
 * <tt>
 * Turn right onto Baker Street and go 1.2 miles.<br> Turn slight left
 * onto MacGregor Street and go 0.8 miles.<br> Turn sharp right onto
 * Burton street and go 2.3 miles.<br> Continue onto Connor Street and
 * go 1.1 miles.<br>
 * </tt>
 * </p>
 *
 * <p>
 * Each line should correspond to a single geographic feature of the
 * route.  In the first line, "Baker Street" is the name of the first
 * geographical feature of the route, and "1.2 miles" is the length of
 * the geographic feature.  The length should be reported to
 * tenth-of-a-mile precision. Each line should be terminated by a
 * newline and should include no extra spaces other than those shown
 * above.
 * </p>
 */
public class DrivingRouteFormatter extends RouteFormatter {

    // You can use this object to round floating point values to one
    // decimal place.
    private static final DecimalFormat formatter = new DecimalFormat("0.0");


    /**
     * <p>
     * Computes a single line of a multi-line directions String that
     * represents the instructions for traversing a single
     * geographical feature.
     * </p>
     *
     * <p>
     * Calling <tt>computeLine</tt> with a GeoFeature instance and an
     * initial heading should produce a newline-terminated String in
     * the following form:
     * </p>
     *
     * <p>
     * <tt>
     * Turn sharp left onto MacGregor Street and go 0.8 miles.<br>
     * </tt>
     * </p>
     *
     * <p>
     * In the output above, "MacGregor Street" represents the name of
     * the geographical feature, and "0.8 miles" is the length of the
     * geographical feature.  The length should be reported to
     * tenth-of-a-mile precision.  The String should be terminated by
     * a newline and should include no extra spaces other than those
     * shown above.
     * </p>
     *
     * @requires 0 &lt;= origHeading &lt; 360
     * @param geoFeature The geographical feature to traverse.
     * @param origHeading The initial heading
     * @return A newline-terminated <tt>String</tt> that gives
     * directions on how to traverse this geographical feature.
     */
    public String computeLine(GeoFeature geoFeature, double origHeading) {
        String line;

        line = getTurnString(origHeading, geoFeature.getStartHeading());
        line += "onto " + geoFeature.getName();
        line += " and go " + formatter.format(geoFeature.getLength()) + " miles.\n";
        return line;
    }
}
