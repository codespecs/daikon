package MapQuick2;

// Based on Matt Deeds' ps4 solution, but with necessary observers added.

import MapQuick.*;

/**
 * Class overview to be written by student.
 */
public class StreetSegment extends GeoSegment
{

  private StreetNumberSet leftNumbers;
  private StreetNumberSet rightNumbers;
  private String leftZip;
  private String rightZip;
  private StreetClassification streetClass;
  private boolean increasingAddresses;

  /**
   * Creates a new StreetSegment from the given arguments.
   *
   * @requires: leftNumbers is disjoint from rightNumbers && all
   * arguments are non-null
   *
   * @param p1 one end of the StreetSegment
   * @param p2 the other end of the StreetSegment
   * @param name the name of the street of which this is a segment
   * @param leftNumbers street numbers on the left side of the street
   * @param rightNumbers street numbers on the right side of the street
   * @param leftZip ZIP code on the left side of the street
   * @param rightZip ZIP code on the right side of the street
   * @param streetClass StreetClassification of this StreetSegment
   * @param increasingAddresses true if addresses increase from p1 to p2
   * <p>
   * The left and right sides of the street are as viewed from p1 to p2.
   */

  public StreetSegment(String name, GeoPoint p1, GeoPoint p2) {
    super(name, p1, p2);
    throw new
      RuntimeException("Street Segment must be created with address info.");
  }

  public StreetSegment(GeoPoint p1, GeoPoint p2, String name,
                       StreetNumberSet leftNumbers,
                       StreetNumberSet rightNumbers,
                       String leftZip, String rightZip,
                       StreetClassification streetClass,
                       boolean increasingAddresses) {
    super(name, p1, p2);
    this.leftNumbers = leftNumbers;
    this.rightNumbers = rightNumbers;
    this.leftZip = leftZip;
    this.rightZip = rightZip;
    this.streetClass = streetClass;
    this.increasingAddresses = increasingAddresses;
  }



  /** Assuming that street numbers on this street are evenly spaced apart,
   * returns the fraction of the distance that the street number sn is
   * from p1.  The return value is a number strictly between 0 and 1.
   * <p>
   * For instance, if one side of this street contains the street
   * numbers 1, 3, and 7, and this.increasingAddresses is true, then
   * those numbers appear .25, .5, and .75 of the way along the
   * street.  If this.increasingAddresses is false, then the numbers
   * appear .75, .5, and .25 of the way along the street.
   * <p>
   * @requires this.containsNumber(sn)
   * @return the fraction of the distance of this segment that is from
   *     p1 to the address sn
   */
  public double fractionDist(int sn) {
    double frac = 0;
    if (leftNumbers.contains(sn)) {
      frac = (double)(leftNumbers.orderStatistic(sn) + 1) /
        (double)(leftNumbers.size() + 1);
    } else if (rightNumbers.contains(sn)) {
      frac =(double)(rightNumbers.orderStatistic(sn) + 1) /
        (double)(rightNumbers.size() + 1);
    }
    if (!increasingAddresses) frac = 1.0 - frac;
    return length()*frac;
  }

  /** @return a new StreetSegment going in the opposite direction from this */
  //@ also_ensures \typeof(\result) == \type(StreetSegment) // uninstrumented
  public GeoSegment reverse()
  {
    return new StreetSegment(p2(), p1(), name(), rightNumbers, leftNumbers,
                             rightZip, leftZip, streetClass,
                             !increasingAddresses);
  }

  /** @return the ZIP code on the left side of the street. */
  public String leftZip() {
    return leftZip;
  }

  /** @return the ZIP code on the right side of the street. */
  public String rightZip() {
    return rightZip;
  }

  /** @return true if the specified number is on either side of the street,
   *    false otherwise. */
  public boolean contains(int num) {
    if (leftNumbers.contains(num))
      return true;
    if (rightNumbers.contains(num))
      return true;
    return false;
  }

  /** @return true if num is on the left side of this, false otherwise. */
  public boolean isOnLeft(int num)
  {
    return leftNumbers.contains(num);
  }

  /** @return true if num is on the right side of this, false otherwise. */
  public boolean isOnRight(int num)
  {
    return rightNumbers.contains(num);
  }

  public String toString()
  {
    return "Street{" +
      super.toString() +
      streetClass + "," +
      increasingAddresses + "," +
      "L(" + leftZip + "," + leftNumbers + ")," +
      "R(" + rightZip + "," + rightNumbers + ")," +
      "}";
  }

}
