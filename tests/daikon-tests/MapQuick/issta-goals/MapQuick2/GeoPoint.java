package MapQuick2;

import MapQuick.*;
import junit.framework.Assert;

/**
 * A GeoPoint models a point on the earth.<p>
 *
 * @specfield  latitude : double          // measured in degrees
 * @specfield  longitude : double         // measured in degrees
 * @endspec
 *
 * <p>GeoPoints are immutable.
 *
 *
 * <p>South latitudes and west longitudes are represented by negative numbers.
 *
 * <p>The code may assume that the represented points are nearby Boston.
 *
 * <p><b>Implementation hint</b>:<br>
 * Boston is at approximately 42 deg. 21 min. 30 sec. N latitude and 71
 * deg. 03 min. 37 sec. W longitude.  At that location, there are
 * approximately 69.023 miles per degree of latitude and 51.075 miles per
 * degree of longitude.  An implementation may use these values when
 * determining distances and headings.
 */
public class GeoPoint {

  /*@ spec_public */ private final static double REP_SCALE_FACTOR = 1000000.0;

  /*@ spec_public */ private final int latitude;
  /*@ spec_public */ private final int longitude;

  /*@ ensures latitude == this.latitude; */
  /*@ ensures longitude == this.longitude; */
  // Constructors

  /**
   * @requires the point given by (latitude, longitude) is near Boston
   * @effects constructs a GeoPoint from a latitude and longitude given in degrees East and North.
   */
  public GeoPoint(int latitude, int longitude)
  {
    this.latitude = latitude;
    this.longitude = longitude;
  }
  /**@ also_ensures \result != null; */
  // Observers

  public String toString() {
    return "Pt{" +
      (latitude / REP_SCALE_FACTOR) +
      "," +
      (longitude / REP_SCALE_FACTOR) +
      "}";
  }

  /**
   * Compares the specified Object with this GeoPoint for equality.
   * @return    gp != null && (gp instanceof GeoPoint)
   *         && gp.latitude == this.latitude && gp.longitude == this.longitude
   */
  public boolean equals(Object o)
  {
    if(!(o instanceof GeoPoint))
      return false;

    GeoPoint other = (GeoPoint) o;
    return
      (this.latitude == other.latitude) &&
      (this.longitude == other.longitude);
  }

  // specified by superclass (Object)
  public int hashCode()
  {
    return latitude * 7 + longitude * 41;
  }

  /*@ requires gp != null; */
  /** Computes the distance between GeoPoints.
   * @requires gp != null
   * @return a close approximation of as-the-crow-flies distance, in
   *         miles, from this to gp
   */
  public double distanceTo(GeoPoint gp)
  {
    Assert.assertNotNull(gp);

    double x = (gp.latitude - this.latitude) * 69.023 / REP_SCALE_FACTOR;
    double y = (gp.longitude - this.longitude) * 51.075 / REP_SCALE_FACTOR;
    return Math.sqrt(x * x + y * y);
  }

  /*@ requires gp != null; */
  /** Computes the compass heading between GeoPoints.
   * @requires gp != null && !this.equals(gp)
   * @return a close approximation of compass heading h from this to
   *         gp, in degrees, such that 0 <= h < 360.  In compass
   *         headings, * north = 0, east = 90, south = 180, and west =
   *         270.
   */
  public double headingTo(GeoPoint gp)
  {
Assert.assertNotNull(gp);
    Assert.assert(!equals(gp));

    double x = (gp.latitude - this.latitude) * 69.023 / REP_SCALE_FACTOR;
    double y = (gp.longitude - this.longitude) * 51.075 / REP_SCALE_FACTOR;

    double angle = Math.toDegrees(Math.atan2(y, x));
    if (angle < 0) {
      angle += 360.0;
    }
    return angle;
  }

} // GeoPoint




