package MapQuick3;

import java.util.*;

/**
 * <p>
 * A Route is a path that traverses arbitrary GeoSegments, regardless of their names. Routes are
 * immutable.
 * </p>
 *
 * <p>
 * Routes are immutable. New Routes can be constructed by adding a segment to the end of a Route. An
 * added segment must be properly oriented; that is, its p1 field must correspond to the end of the
 * original Route, and its p2 field corresponds to the end of the new Route.
 * </p>
 *
 * <p>
 * Because a Route is not necessarily straight, its length -- the distance traveled by following the
 * path from start to end -- is not necessarily the same as the distance along a straight line
 * between its endpoints.
 * </p>
 *
 * <p>
 * Lastly, a Route may be viewed as a sequence of geographical features, using the
 * <tt>getGeoFeatures()</tt> method which returns an Iterator<GeoFeature>.
 * </p>
 *
 * @specfield start : GeoPoint // location of the start of the route
 * @specfield end : GeoPoint // location of the end of the route
 * @specfield startHeading : angle // direction of travel at the start of the route, in degrees
 * @specfield endHeading : angle // direction of travel at the end of the route, in degrees
 * @specfield geoFeatures : sequence // a sequence of geographic features that make up this Route
 * @specfield geoSegments : sequence // a sequence of segments that make up this Route
 * @derivedfield length : real // total length of the route, in miles
 */
public class Route {
    // FIELDS

    private final List<GeoSegment> geoSegments;
    private final double length;

    // Abstraction Function:
    // Let "last" be the index of the last element of geoSegments (i.e. last = geoSegments.size()-1)
    // For a realworld route, route, and a Route r:
    // route.geoSegments = r.geoSegments
    // route.start = r.geoSegments[0].p1
    // route.end = r.geoSegments[last].p2
    // route.startHeading = r.geoSegments[0].getHeading()
    // route.endHeading = r.geoSegments[last].getHeading()
    // route.geoFeatures is derived from r.geoSegments. All adjacent GeoSegments that have the same
    //   name are added into a single GeoFeature of that name that starts with the first segment and
    //   ends with the last of the adjacent segments. Then route.geoFeatures is the sequence created
    //   by iteratring over these GeoFeatures in the order that their first GeoSegment appeared in
    //   r.geoSegments.

    // Representation Invariant:
    // r.length = sum for 0 <= i <= last of geoSegments[i].length
    // geoSegments[i] != null for 0 <= i <= last
    // geoSegments[i].p2 = geoSegments[i+1].p1 for 0 <= i <= last
    // geoFeatures[i].name != geoFeatures[i+1].name for all 0 <= i < geoFeatures.size()-1

    // Constructors

    /**
     * @requires gs != null
     * @effects Constructs a new Route.
     */
    public Route(GeoSegment gs) {
        geoSegments = new ArrayList<GeoSegment>();
        geoSegments.add(gs);
        length = gs.getLength();
        checkRep();
    }

    // private constructor used by the addSegment() method
    private Route(List<GeoSegment> geoSegments, double length) {
        this.geoSegments = new ArrayList<GeoSegment>(geoSegments);
        this.length = length;
        checkRep();
    }

    /**
     * ensures that the representation invariant holds (if any), and throws a RuntimeException if
     * not.
     */
    private void checkRep() throws RuntimeException {
        double lengthSum = 0;
        GeoSegment gs;
        if (geoSegments == null) throw new RuntimeException("geoSegments cannot be null");
        else {
            for (int i = 0; i < geoSegments.size(); i++) {
                gs = geoSegments.get(i);
                if (gs == null) throw new RuntimeException(
                        "geoSegments cannot have any null elements");
                else if (i < geoSegments.size() - 1
                        && !gs.getP2().equals(geoSegments.get(i + 1).getP1())) throw new RuntimeException(
                        "the endpoint of geoSegments[" + i + "] does not "
                                + "match the start point of the next geoSegment");
                else lengthSum += gs.getLength();
            }
            if (this.length != lengthSum) throw new RuntimeException(
                    "length is not equal to sum of the lengths in geoSegments");
        }
    }

    // Observers

    /**
     * @return location of the start of the route
     */
    public GeoPoint getStart() {
        checkRep();
        return this.geoSegments.get(0).getP1();
    }

    /**
     * @return location of the end of of the route
     */
    public GeoPoint getEnd() {
        checkRep();
        return this.geoSegments.get(geoSegments.size() - 1).getP2();
    }

    /**
     * @requires geoSegments[0].length > 0 (i.e. first segment must have length > 0)
     * @return direction (in compass heading) of travel at the start of the route, in degrees
     */
    public double getStartHeading() {
        checkRep();
        return this.geoSegments.get(0).getHeading();
    }

    /**
     * @requires geoSegments[geoSegments.size -1].length > 0 (i.e. the last segment must have length >
     *           0)
     * @return direction of travel at the end of the route, in degrees
     */
    public double getEndHeading() {
        checkRep();
        return this.geoSegments.get(geoSegments.size() - 1).getHeading();
    }

    /**
     * @return total length of the route, in miles. NOTE: this is NOT as-the-crow-flies, but rather
     *         the total distance required to traverse the route. These values are not necessarily
     *         equal.
     */
    public double getLength() {
        return this.length;
    }

    // Producers

    /**
     * Creates a new route that is equal to this route with gs appended to its end.
     *
     * @requires gs != null && gs.p1 == this.end
     * @return a new Route r such that r.end = gs.p2 && r.endHeading = gs.heading && r.length =
     *         this.length + gs.length
     */
    public Route addSegment(GeoSegment gs) {
        List<GeoSegment> newGeoSegments = new ArrayList<GeoSegment>(this.geoSegments);
        newGeoSegments.add(gs);

        checkRep();
        return new Route(newGeoSegments, this.length + gs.getLength());
    }

    /**
     * Returns an Iterator sequence of GeoFeature objects. The concatenation of the GeoFeatures, in
     * order, is equivalent to this route. No two consecutive GeoFeature objects have the same name.
     * <p>
     *
     * @return an Iterator of GeoFeatures such that
     *
     * <pre>
     *           this.start        = a[0].start
     *        &amp;&amp; this.startHeading = a[0].startHeading
     *        &amp;&amp; this.end          = a[a.length - 1].end
     *        &amp;&amp; this.endHeading   = a[a.length - 1].endHeading
     *        &amp;&amp; this.length       =  sum (0 &lt;= i &lt; a.length) . a[i].length
     *        &amp;&amp; for all integers i .
     *               (0 &lt;= i &lt; a.length - 1 =&gt; (a[i].name != a[i+1].name &amp;&amp;
     *                                            a[i].end   = a[i+1].start))
     * </pre>
     *
     * where <code>a[n]</code> denotes the nth element of the Iterator.
     * @see MapQuick3.GeoFeature
     */
    public Iterator<GeoFeature> getGeoFeatures() {
        checkRep();

        GeoSegment gs;
        List<GeoFeature> features = new ArrayList<GeoFeature>();
        Iterator<GeoSegment> iter = this.geoSegments.iterator();

        // the first GeoFeature will always have the first element of geoSegments
        GeoFeature gf = new GeoFeature(iter.next());

        while (iter.hasNext()) {
            gs = iter.next();
            // if the next GeoSegment has the same name as the current GeoFeature
            // then add it to the current GeoFeature
            if (gs.getName() == gf.getName()) gf = gf.addSegment(gs);
            // if it has a different name then add this GeoFeature
            // and create a new one
            else {
                features.add(gf);
                gf = new GeoFeature(gs);
            }
        }
        features.add(gf); // add the last GeoFeature created
        checkRep();
        return features.iterator();
    }


    /**
     * Returns an Iterator of GeoSegment objects. The concatentation of the GeoSegments, in order,
     * is equivalent to this route.
     *
     * @return an Iterator of GeoSegments such that
     *
     * <pre>
     *           this.start        = a[0].start
     *        &amp;&amp; this.startHeading = a[0].startHeading
     *        &amp;&amp; this.end          = a[ a.length - 1 ].end
     *        &amp;&amp; this.endHeading   = a[ a.length - 1 ].endHeading
     *        &amp;&amp; this.length       =  sum (0 &lt;= i &lt; a.length) . a[i].length
     *
     * </pre>
     *
     * where <code>a[n]</code> denotes the nth element of the Iterator.
     * @see MapQuick3.GeoSegment
     */
    public Iterator<GeoSegment> getGeoSegments() {
        checkRep();
        return this.geoSegments.iterator();
    }

    /**
     * Compares the specified Object with this Route for equality.
     *
     * @return true iff (o instanceof Route) && (o.geoFeatures and this.geoFeatures contain the same
     *         elements in the same order).
     */
    public boolean equals(Object o) {
        if (o != null && o instanceof Route) {
            Iterator<GeoFeature> iterThis = this.getGeoFeatures();
            Iterator<GeoFeature> iterOther = ((Route) o).getGeoFeatures();
            while (iterThis.hasNext() && iterOther.hasNext()) {
                if (!iterThis.next().equals(iterOther.next())) return false;
            }
            // Make sure that both iterators have no more elements
            if (iterThis.hasNext() || iterOther.hasNext()) return false;
            // All elements matched
            else return true;
        }
        // null or not instanceof Route
        else return false;
    }

    /**
     * @return a valid hash code for this.
     */
    public int hashCode() {
        // This implementation will work, but you may want to modify
        // it later for improved performance. If you do change the
        // implementation, make sure it satisfies the hashCode
        // invariant. That is, if equals returns true for two
        // objects, then they must have the same hashCode.

        return (1);
    }

    /**
     * @return a string representation of this.
     */
    public String toString() {
        // "[GeoFeature1, GeoFeature2, ...]"

        Iterator<GeoFeature> iter = this.getGeoFeatures();
        String retString = "[";
        while (iter.hasNext()) {
            retString += iter.next().toString() + ", ";
        }
        retString = retString.substring(0, retString.lastIndexOf(',')) + "]";
        return retString;
    }

} // Route
