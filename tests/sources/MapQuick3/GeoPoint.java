package MapQuick3;

/**
 * <p>
 * A GeoPoint models a point on the earth. GeoPoints are immutable.
 * </p>
 *
 * <p>
 * North latitudes and east longitudes are represented by positive numbers. South latitudes and west
 * longitudes are represented by negative numbers.
 * </p>
 *
 * <p>
 * The code may assume that the represented points are nearby Boston.
 * </p>
 *
 * <p>
 * <b>Implementation hint</b>:<br>
 * Boston is at approximately 42 deg. 21 min. 30 sec. N latitude and 71 deg. 03 min. 37 sec. W
 * longitude. There are 60 minutes per degree, and 60 seconds per minute. So, in decimal, these
 * correspond to 42.358333 North latitude and -71.060278 East longitude. The constructor takes
 * integers in millionths of degrees. To create a new GeoPoint located in Boston, use:
 * <tt>GeoPoint boston = new
 * GeoPoint(42358333, -71060278);</tt>
 * </p>
 *
 * <p>
 * Near Boston, there are approximately 69.023 miles per degree of latitude and 51.075 miles per
 * degree of longitude. An implementation should use these values when determining distances and
 * headings.
 * </p>
 *
 * @specfield latitude : real // measured in degrees latitude
 * @specfield longitude : real // measured in degrees longitude
 */
public class GeoPoint {

    /** Minimum value the latitude field can have in this class. * */
    public static final int MIN_LATITUDE = -90 * 1000000;

    /** Maximum value the latitude field can have in this class. * */
    public static final int MAX_LATITUDE = 90 * 1000000;

    /** Minimum value the longitude field can have in this class. * */
    public static final int MIN_LONGITUDE = -180 * 1000000;

    /** Maximum value the longitude field can have in this class. * */
    public static final int MAX_LONGITUDE = 180 * 1000000;

    /**
     * Approximation used to determine distances and headings using a "flat earth" simplification.
     */
    public static final double MILES_PER_DEGREE_LATITUDE = 69.023;

    /**
     * Approximation used to determine distances and headings using a "flat earth" simplification.
     */
    public static final double MILES_PER_DEGREE_LONGITUDE = 51.075;

    // FIELDS

    private final int latitude;
    private final int longitude;

    // Abstraction Function:
    // For a point, p, on Earth and a GeoPoint, r, r.latitude and r.longitude are integers
    // representing p.latitude and p.longitude respectively in millionths of degrees. That is:
    // p.latitude = r.latitude/1000000
    // p.longitude = r.longitude/1000000

    // Representation Invariant:
    // MIN_LATITUDE <= r.latitude <= MAX_LATITUDE
    // MIN_LONGITUDE <= r.longitude <= MAX_LONGITUDE

    // Constructors

    /**
     * @requires the point given by (latitude, longitude) in millionths of degrees is near Boston
     *
     * @effects constructs a GeoPoint from a latitude and longitude given in millionths of degrees.
     */
    public GeoPoint(int latitude, int longitude) {

        this.latitude = latitude;
        this.longitude = longitude;

        checkRep();
    }

    /**
     * ensures that the representation invariant holds (if any), and throws a RuntimeException if
     * not.
     */
    private void checkRep() throws RuntimeException {

        if (!(MIN_LATITUDE <= this.latitude || this.latitude <= MAX_LATITUDE)) throw new RuntimeException(
                "Latitude is out of acceptable range");

        if (!(MIN_LONGITUDE <= this.longitude || this.longitude <= MAX_LONGITUDE)) throw new RuntimeException(
                "Longitude is out of acceptable range");
    }

    // Observers

    /**
     * the latitude of the GeoPoint object, in millionths of degrees.
     */
    public int getLatitude() {
        checkRep();
        return this.latitude;
    }

    /**
     * the longitude of the GeoPoint object, in millionths of degrees.
     */
    public int getLongitude() {
        checkRep();
        return this.longitude;
    }

    /**
     * Computes the distance between GeoPoints.
     *
     * @requires gp != null
     * @return The distance, in miles, from this to gp. For relatively short distances, the
     *         straight-line distance is very close to the smallest arc connecting the two points on
     *         a sphere, so this implementation may simply return the straight-line distance.
     */
    public double distanceTo(GeoPoint gp) {
        // Distance (x1,y1) to (x2,y2) = sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

        double latDist, longDist, distance;

        checkRep();

        // Convert difference in degrees to distance in miles
        latDist = (this.latitude - gp.latitude) * MILES_PER_DEGREE_LATITUDE / 1000000;
        longDist = (this.longitude - gp.longitude) * MILES_PER_DEGREE_LONGITUDE / 1000000;
        distance = Math.sqrt(latDist * latDist + longDist * longDist);

        checkRep();

        return distance;
    }

    /**
     * Computes the compass heading between GeoPoints.
     *
     * @requires gp != null && !this.equals(gp)
     * @return a close approximation of compass heading h from this to gp, in degrees, using the
     *         flat-surface, near Boston approximation, such that 0 &lt;= h &lt; 360. In compass
     *         headings, north = 0, east = 90, south = 180, and west = 270.
     */
    public double headingTo(GeoPoint gp) {
        // Heading (x1,y1) to (x2,y2) = arctan((x2-x1)/(y2-y1))

        double latDist, longDist, heading;

        checkRep();

        // Convert difference in degrees to distance in miles
        latDist = (gp.latitude - this.latitude) * MILES_PER_DEGREE_LATITUDE / 1000000;
        longDist = (gp.longitude - this.longitude) * MILES_PER_DEGREE_LONGITUDE / 1000000;
        heading = Math.toDegrees(Math.atan2(longDist, latDist));
        // Convert (-180 <= heading < 0) to (180 <= heading < 360;)
        if (heading < 0) heading += 360;

        checkRep();
        return heading;
    }

    /**
     * Compares the specified Object with this GeoPoint for equality.
     *
     * @return gp != null && (gp instanceof GeoPoint) && gp.latitude = this.latitude && gp.longitude =
     *         this.longitude
     */
    public boolean equals(Object gp) {
        checkRep();
        GeoPoint point;
        if (gp != null && (gp instanceof GeoPoint)) {
            point = (GeoPoint) gp;
            return (this.latitude == point.latitude) && (this.longitude == point.longitude);
        } else return false;
    }

    /**
     * @return a valid hashcode for this GeoPoint.
     */
    public int hashCode() {
        // This implementation will work, but you may want to modify
        // it later for improved performance. If you do change the
        // implementation, make sure it satisfies the hashCode
        // invariant. That is, if equals returns true for two
        // objects, then they must have the same hashCode.
        checkRep();
        Integer temp = 17 * this.latitude + 23 * this.longitude;
        return temp.hashCode();
    }

    /**
     * @return a string representation of this GeoPoint.
     */
    public String toString() {
        // (this.latitude,this.longitude)
        return "(" + this.latitude + ", " + this.longitude + ")";
    }

} // GeoPoint
