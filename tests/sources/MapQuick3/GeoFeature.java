package MapQuick3;

import java.util.*;

/**
 * <p>
 * A GeoFeature represents a path from one location to another along a single geographic feature.
 * GeoFeatures are immutable.
 * </p>
 *
 * <p>
 * GeoFeature abstracts over a sequence of GeoSegments, all of which have the same name, thus
 * providing a representation for nonlinear or nonatomic geographic features. As an example, a
 * GeoFeature might represent the course of a winding river, or travel along a road through
 * intersections but remaining on the same road.
 * </p>
 *
 * <p>
 * GeoFeatures are immutable. New GeoFeatures can be constructed by adding a segment to the end of a
 * GeoFeature. An added segment must be properly oriented; that is, its p1 field must correspond to
 * the end of the original GeoFeature, and its p2 field corresponds to the end of the new
 * GeoFeature, and the name of the GeoSegment being added must match the name of the existing
 * GeoFeature.
 * </p>
 *
 * <p>
 * Because a GeoFeature is not necessarily straight, its length -- the distance traveled by
 * following the path from start to end -- is not necessarily the same as the distance along a
 * straight line between its endpoints.
 * </p>
 *
 * @specfield start : GeoPoint // location of the start of the geo feature
 * @specfield end : GeoPoint // location of the end of the geo feature
 * @specfield startHeading : angle // direction of travel at the start of the geo feature, in
 *            degrees
 * @specfield endHeading : angle // direction of travel at the end of the geo feature, in degrees
 * @specfield geoSegments : sequence // a sequence of segments that make up this geographic feature
 * @specfield name : String // name of geographical feature
 * @derivedfield length : real // total length of the geo feature, in miles
 *
 */
public class GeoFeature {

    // FIELDS

    private final List<GeoSegment> geoSegments;
    private final double length;

    // Abstraction Function:
    // Let "last" be the index of the last element of geoSegments (i.e. last = geoSegments.size()-1)
    // For a geographic feature, gf, and a GeoFeature r:
    // gf.start = r.geoSegments[0].p1
    // gf.end = r.geoSegments[last].p2
    // gf.startHeading = r.geoSegments[0].getHeading()
    // gf.endHeading = r.geoSegments[last].getHeading()
    // gf.name = r.geoSegments[0].name
    // gf.geoSegments = r.geoSegments

    // Representation Invariant:
    // r.length = sum for 0 <= i <= last of geoSegments[i].length
    // geoSegments[i] != null for 0 <= i <= last
    // geoSegments[i].name = geoSegments[j].name for all 0 <= i,j <= last
    // geoSegments[i].p2 = geoSegments[i+1].p1 for 0 <= i <= last

    // Constructors

    /**
     * Constructs a new GeoFeature.
     *
     * @requires gs != null
     * @effects Constructs a new GeoFeature, r, such that r.name = gs.name && r.startHeading =
     *          gs.heading && r.endHeading = gs.heading && r.start = gs.p1 && r.end = gs.p2
     */
    public GeoFeature(GeoSegment gs) {
        geoSegments = new ArrayList<GeoSegment>();
        geoSegments.add(gs);
        length = gs.getLength();
        checkRep();
    }

    // private constructor used by the addSegment() method
    private GeoFeature(List<GeoSegment> geoSegments, double length) {
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
        String name;
        GeoSegment gs;
        if (geoSegments == null)
            throw new RuntimeException("geoSegments cannot be null");
        else {
            name = geoSegments.get(0).getName();
            for (int i = 0; i < geoSegments.size(); i++) {
                gs = geoSegments.get(i);
                if (gs == null)
                    throw new RuntimeException("geoSegments cannot have any null elements");
                else if (gs.getName() != name)
                    throw new RuntimeException("all GeoSegments must have the same name");
                else if (i < geoSegments.size()-1 && !gs.getP2().equals(geoSegments.get(i+1).getP1()))
                    throw new RuntimeException("the endpoint of geoSegments[" + i + "] does not " +
                            "match the start point of the next geoSegment");
                else lengthSum += gs.getLength();
            }
            if (this.length != lengthSum)
                throw new RuntimeException("length is not equal to sum of the lengths in geoSegments");
        }
    }

    // Observers

    /**
     * @return name of geographic feature
     */
    public String getName() {
        checkRep();
        return this.geoSegments.get(0).getName();
    }

    /**
     * @return location of the start of the feature
     */
    public GeoPoint getStart() {
        checkRep();
        return this.geoSegments.get(0).getP1();
    }

    /**
     * @return location of the end of of the feature
     */
    public GeoPoint getEnd() {
        checkRep();
        return this.geoSegments.get(geoSegments.size()-1).getP2();
    }

    /**
     * @requires geoSegments[0].length > 0 (i.e. first segment must have length > 0)
     * @return direction (in standard heading) of travel at the start of the feature, in degrees
     */
    public double getStartHeading() {
        checkRep();
        return this.geoSegments.get(0).getHeading();
    }

    /**
     * @requires geoSegments[geoSegments.size -1].length > 0 (i.e. the last segment must have length >
     *           0)
     * @return direction of travel at the end of the feature, in degrees
     */
    public double getEndHeading() {
        checkRep();
        return this.geoSegments.get(geoSegments.size()-1).getHeading();
    }

    /**
     * @return total length of the geo feature, in miles. NOTE: this is NOT as-the-crow-flies, but
     *         rather the total distance required to traverse the geo feature. These values are not
     *         necessarily equal.
     */
    public double getLength() {
        checkRep();
        return this.length;
    }

    // Producers

    /**
     * Creates a new GeoFeature that is equal to this GeoFeature with gs appended to its end.
     *
     * @requires gs != null && gs.p1 = this.end && gs.name = this.name
     * @return a new GeoFeature r such that r.end = gs.p2 && r.endHeading = gs.heading && r.length =
     *         this.length + gs.length
     */
    public GeoFeature addSegment(GeoSegment gs) {

        List<GeoSegment> newGeoSegments = new ArrayList<GeoSegment>(this.geoSegments);
        newGeoSegments.add(gs);

        checkRep();
        return new GeoFeature(newGeoSegments, this.length + gs.getLength());
    }

    // Observers

    /**
     * Returns an Iterator of GeoSegment objects. The concatentation of the GeoSegments, in order,
     * is equivalent to this GeoFeature. All the GeoSegments should have the same name.
     *
     * @return an Iterator of GeoSegments such that
     *
     * <pre>
     *         this.start        = a[0].start
     *      &amp;&amp; this.startHeading = a[0].startHeading
     *      &amp;&amp; this.end          = a[ a.length - 1 ].end
     *      &amp;&amp; this.endHeading   = a[ a.length - 1 ].endHeading
     *      &amp;&amp; this.length       =  sum (0 &lt;= i &lt; a.length) . a[i].length
     *      &amp;&amp; for all integers i .
     *             (0 &lt;= i &lt; a.length - 1 =&gt; (a[i].name == a[i+1].name &amp;&amp;
     *                                                 a[i].end == a[i+1].start))
     * </pre>
     *
     * where <code>a[n]</code> denotes the nth element of the Iterator.
     * @see MapQuick3GeoSegment
     */
    public Iterator<GeoSegment> getGeoSegments() {
        checkRep();
        return this.geoSegments.iterator();
    }

    /**
     * Compares the argument with this GeoFeature for equality.
     *
     * @return o != null && (o instanceof GeoFeature) && (o.geoSegments and this.geoSegments contain
     *         the same elements in the same order).
     */
    public boolean equals(Object o) {
        if (o != null && o instanceof GeoFeature) {
            Iterator<GeoSegment> iterThis = this.getGeoSegments();
            Iterator<GeoSegment> iterOther = ((GeoFeature) o).getGeoSegments();
            while (iterThis.hasNext() && iterOther.hasNext()) {
                if (!iterThis.next().equals(iterOther.next()))
                    return false;
            }
            // Make sure that both iterators have no more elements
            if (iterThis.hasNext() || iterOther.hasNext())
                return false;
            // All elements matched
            else return true;
        }
        // null or not instanceof GeoFeature
        else return false;
    }

    /**
     * @return a valid hashcode for this.
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
        return this.getName();
    }

} // GeoFeature
