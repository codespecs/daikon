package MapQuick3;

/**
 * <p>
 * A GeoSegment models a straight line segment on the earth.
 * GeoSegments are immutable.
 * </p>
 *
 * <p>
 * A compass heading is a nonnegative real number less than 360.  In
 * compass headings, north = 0, east = 90, south = 180, and west =
 * 270.
 * </p>
 *
 * <p>
 * When used in a map, a GeoSegment might represent part of a street,
 * boundary, or other feature.
 * As an example usage, this map
 * <pre>
 *  Penny Lane  a
 *              |
 *              i--j--k  Abbey Road
 *              |
 *              z
 * </pre>
 * could be represented by the following GeoSegments:
 * ("Penny Lane", a, i), ("Penny Lane", z, i),
 * ("Abbey Road", i, j), and ("Abbey Road", j, k).
 * </p>
 *
 * <p>
 * A name is given to all GeoSegment objects so that it is possible to
 * differentiate between two GeoSegment objects with identical
 * GeoPoint endpoints.  Equality between GeoSegment objects requires
 * that the names be equal String objects and the end points be equal
 * GeoPoint objects.
 * </p>
 *
 * @specfield  name : String       // name of the geographic feature identified
 * @specfield  p1 : GeoPoint       // first endpoint of the segment
 * @specfield  p2 : GeoPoint       // second endpoint of the segment
 * @derivedfield length : real     // straight-line distance between p1 and p2, in miles
 * @derivedfield  heading : angle  // compass heading from p1 to p2, in degrees
 */
public class GeoSegment  {

    // FIELDS

    private final String name;
    private final GeoPoint p1;
    private final GeoPoint p2;

    // Abstraction Function:
    // For a straight line segment, l, on Earth and a GeoSegment r:
    // l.name = r.name
    // l.p1 = r.p1
    // l.p2 = r.p2

    // Representation Invariant:
    // 0 <= r.heading < 360
    // r.length >= 0
    //

    // Constructors

    /**
     * @requires name != null && p1 != null && p2 != null
     * @effects constructs a new GeoSegment with the specified
     * name and endpoints
     */
    public GeoSegment(String name, GeoPoint p1, GeoPoint p2) {
        this.name = name;
        this.p1 = p1;
        this.p2 = p2;
        checkRep();
    }


    /**
     * ensures that the representation invariant holds (if any), and
     * throws a RuntimeException if not.
     */
    private void checkRep() throws RuntimeException {
        if (this.name == null)
            throw new RuntimeException("name cannot be null");
        else if (this.p1 == null)
            throw new RuntimeException("p1 cannot be null");
        else if (this.p2 == null)
            throw new RuntimeException("p2 cannot be null");
        else if (this.p1.distanceTo(this.p2) < 0)
            throw new RuntimeException("length must be >= 0");
        else if (this.p1.headingTo(this.p2) < 0 || this.p1.headingTo(this.p2) >= 360)
            throw new RuntimeException("heading must be between 0 and 360");
    }


    // Producers

    /**
     * Returns a new GeoSegment like this one, but with its endpoints
     * reversed.
     * @return a new GeoSegment gs such that
     *      gs.name = this.name
     *   && gs.p1 = this.p2
     *   && gs.p2 = this.p1
     */
    public GeoSegment reverse() {
        checkRep();
        return new GeoSegment(this.name, this.p2, this.p1);
    }


    // Observers

    /**
     * @return the name of this GeoSegment.
     */
    public String getName() {
        checkRep();
        return this.name;
    }


    /**
     * @return first endpoint of the segment.
     */
    public GeoPoint getP1() {
        checkRep();
        return this.p1;
    }


    /**
     * @return second endpoint of the segment.
     */
    public GeoPoint getP2() {
        checkRep();
        return this.p2;
    }


    /**
     * @return the length of the segment.
     */
    public double getLength() {
        checkRep();
        return p1.distanceTo(p2);
    }


    /**
     * @requires this.length != 0
     * @return the compass heading from p1 to p2, in degrees.
     */
    public double getHeading() {
        checkRep();
        return p1.headingTo(p2);
    }


    /**
     * Compares the specified Object with with this GeoSegment for
     * equality.
     * @return    gs != null && (gs instanceof GeoSegment)
     *         && gs.name = this.name && gs.p1 = this.p1 && gs.p2 = this.p2
     */
    public boolean equals(Object gs) {
        if (gs != null && (gs instanceof GeoSegment)) {
            GeoSegment seg = (GeoSegment)gs;
            return (this.name.equals(seg.getName()) && this.p1.equals(seg.getP1())
                    && this.p2.equals(seg.getP2()));
        }
        else return false;
    }


    /**
     * @return a valid hashcode for this.
     */
    public int hashCode() {
        // This implementation will work, but you may want to modify
        // it later for improved performance.  If you do change the
        // implementation, make sure it satisfies the hashCode
        // invariant.  That is, if equals returns true for two
        // objects, then they must have the same hashCode.
        return (3*this.name.hashCode()+5*this.p1.hashCode()+7*this.p2.hashCode());
    }


    /**
     * @return a string representation of this.
     */
    public String toString() {
        // "[name, p1, p2]"
        return "[" + this.name + ", " + this.p1.toString() + ", " + this.p2.toString() + "]";
    }

} // GeoSegment
