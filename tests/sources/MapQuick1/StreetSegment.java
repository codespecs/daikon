package MapQuick1;

import MapQuick.*;
import junit.framework.Assert;

/**
 * Class overview to be written by student.
 */
public class StreetSegment extends GeoSegment
{

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
  public StreetSegment(GeoPoint p1, GeoPoint p2, String name,
		       StreetNumberSet leftNumbers, StreetNumberSet rightNumbers,
		       String leftZip, String rightZip,
		       StreetClassification streetClass,
		       boolean increasingAddresses)
  {
    super(name, p1, p2);

    // requires clause
    Assert.assertNotNull(p1);
    Assert.assertNotNull(p2);
    Assert.assertNotNull(name);
    Assert.assertNotNull(leftNumbers);
    Assert.assertNotNull(rightNumbers);
    Assert.assertNotNull(leftZip);
    Assert.assertNotNull(rightZip);
    Assert.assertNotNull(streetClass);
    Assert.assertTrue(!leftNumbers.intersects(rightNumbers));

    // store the fields
    this.leftNumbers = leftNumbers;
    this.rightNumbers = rightNumbers;
    this.leftZip = leftZip;
    this.rightZip = rightZip;
    this.streetClass = streetClass;
    this.incAddr = increasingAddresses;

    checkRep();
  }

  private final StreetNumberSet leftNumbers;
  private final StreetNumberSet rightNumbers;
  private final String leftZip;
  private final String rightZip;
  private final StreetClassification streetClass;
  private final boolean incAddr;

  public String leftZip() { return leftZip; }
  public String rightZip() { return rightZip; }
  public boolean isOnLeft(int n) { return leftNumbers.contains(n); }
  public boolean isOnRight(int n) { return rightNumbers.contains(n); }
  public boolean contains(int n) { return isOnLeft(n) || isOnRight(n); }

  public void checkRep()
  {
    Assert.assertNotNull(leftNumbers);
    Assert.assertNotNull(rightNumbers);
    Assert.assertNotNull(leftZip);
    Assert.assertNotNull(rightZip);
    Assert.assertNotNull(streetClass);
  }

  // Specified by superclass
  public GeoSegment reverse()
  {
    return new StreetSegment(p2(), p1(), name(),
			     rightNumbers, leftNumbers,
			     rightZip, leftZip,
			     streetClass,
			     !incAddr);
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
  public double fractionDist(int n)
  {
    checkRep();

    boolean inL = leftNumbers.contains(n);
    boolean inR = rightNumbers.contains(n);
    Assert.assertTrue(inL ^ inR);

    StreetNumberSet sns;
    if (inL) {
      sns = leftNumbers;
    } else {
      sns = rightNumbers;
    }

    int stat = sns.orderStatistic(n);
    int size = sns.size();
    double result = ((double) (stat + 1)) / (size + 1);
    if (!incAddr) {
      result = 1 - result;
    }

    Assert.assertTrue((0.0 <= result) && (result <= 1.0));
    checkRep();
    return result;
  }

}
