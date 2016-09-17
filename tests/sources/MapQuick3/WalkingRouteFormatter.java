package MapQuick3;



/**
 * <p>
 * A WalkingRouteFormatter class knows how to create a textual
 * description of directions from one location to another suitable for
 * a pedestrian.
 * </p>
 *
 * <p>
 * Calling <tt>computeDirections</tt> should produce directions in the
 * following form:
 * </p>
 *
 * <p>
 * <pre>
 * Turn right onto Baker Street and walk for 20 minutes.
 * Turn slight left onto MacGregor Street and walk for 8 minutes.
 * Turn sharp right onto Burton street and walk for 3 minutes.
 * Continue onto Connor Street and walk for 12 minutes.
 * </pre>
 * </p>
 *
 * <p>
 * Each line should correspond to a single geographic feature of the
 * route.  In the first line, "Baker Street" is the name of the first
 * geographical feature of the route, and "20 minutes" is the length
 * of time that it would take to walk along the geographic feature,
 * assuming a walking speed of 20 minutes per mile.  The time in
 * minutes should be reported to the nearest minute. Each line should
 * be terminated by a newline and should include no extra spaces other
 * than those shown above.
 * </p>
 */
public class WalkingRouteFormatter extends RouteFormatter {

    /**
     * <p>
     * Computes a single line of a multi-line directions String that
     * represents the instructions for walking along a single
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
     * <pre>
     * Turn sharp left onto MacGregor Street and walk for 18 minutes.
     * </pre>
     * </p>
     *
     * <p>
     * In the output above, "MacGregor Street" represents the name of
     * the geographical feature, and "18 minutes" is the length of
     * time that it would take to walk along the geographic feature,
     * assuming a walking speed of 20 minutes per mile.  The time in
     * minutes should be reported to the nearest minute.  Each line
     * should be terminated by a newline and should include no extra
     * spaces other than those shown above.
     * </p>
     *
     * @requires 0 &lt;= origHeading &lt; 360
     * @param geoFeature The geographical feature to traverse.
     * @param origHeading The initial heading
     * @return A newline-terminated <tt>String</tt> that gives
     * directions on how to walk along this geographical feature.
     */
    public String computeLine(GeoFeature geoFeature, double origHeading) {
        String line;
        long time = Math.round(geoFeature.getLength()*20);
        line = getTurnString(origHeading, geoFeature.getStartHeading());
        line += "onto " + geoFeature.getName();
        line += " and walk for " + time + " minutes.\n";
        return line;
    }
}
