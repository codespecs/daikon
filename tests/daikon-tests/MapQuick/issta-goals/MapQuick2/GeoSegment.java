package MapQuick2;

import MapQuick.*;
import junit.framework.Assert;

/**
 * A GeoSegment models a straight line segment on the earth.<p>
 *
 * @specfield  name : String       // name of the geographic feature identified
 * @specfield  p1 : GeoPoint       // first endpoint of the segment
 * @specfield  p2 : GeoPoint       // second endpoint of the segment
 * @derivedfield  length : double  // straight-line distance between p1 and p2,
 *                                 // in miles
 * @derivedfield  heading : double // compass heading from p1 to p2, in degrees
 * @endspec
 * <p>
 * GeoSegments are immutable.<p>
 *
 * The choice of which endpoint is p1 and which is p2 is arbitrary.<p>
 *
 * A compass heading is a nonnegative real number less than 360.
 * In compass headings, north = 0, east = 90, south = 180, and west = 270.<p>
 *
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
 */
public class GeoSegment  {

  /*@ invariant this.name != null; */
  /*@ invariant this.p1 != null; */
  /*@ invariant this.p2 != null; */
  // fields
  /*@ spec_public */ final private String name;
  /*@ spec_public */ final private GeoPoint p1;
  /*@ spec_public */ final private GeoPoint p2;

  /*@ requires name != null; */
  /*@ requires p1 != null; */
  /*@ requires p2 != null; */
  /*@ ensures name == this.name; */
  /*@ ensures p1 == this.p1; */
  /**@ ensures p1.latitude == this.p1.latitude; */
  /**@ ensures p1.longitude == this.p1.longitude; */
  /*@ ensures p2 == this.p2; */
  /**@ ensures p2.latitude == this.p2.latitude; */
  /**@ ensures p2.longitude == this.p2.longitude; */
  // Constructors

  /**
   * @requires name != null && p1 != null && p2 != null
   * @effects constructs a new GeoSegment with the specified name and endpoints
   */
  public GeoSegment(String name, GeoPoint p1, GeoPoint p2)
  {
    Assert.assertNotNull(name);
    Assert.assertNotNull(p1);
    Assert.assertNotNull(p2);

    this.name = name;
    this.p1 = p1;
    this.p2 = p2;
  }

  // Producers

  //@ ensures \result != null // sample feeding
  /**
   * Returns a new GeoSegment like this one, but with its endpoints reversed.
   * @return a new GeoSegment gs such that
   *      gs.name = this.name
   *   && gs.p1 = this.p2
   *   && gs.p2 = this.p1
   */
  public GeoSegment reverse()
  {
    return new GeoSegment(name, p2, p1);
  }

  /*@ ensures \result == this.name; */
  /**@ ensures \result == \old(this.name); */
  // Observers

  /** @return this.name */
  public String name()
  {
    return this.name;
  }

  /*@ ensures \result == this.p1; */
  /**@ ensures \result == \old(this.p1); */
  /**@ ensures \result.latitude == this.p1.latitude; */
  /**@ ensures \result.latitude == \old(this.p1.latitude); */
  /**@ ensures \result.longitude == this.p1.longitude; */
  /**@ ensures \result.longitude == \old(this.p1.longitude); */
  /** @return this.p1 */
  public GeoPoint p1()
  {
    return this.p1;
  }

  /*@ ensures \result == this.p2; */
  /**@ ensures \result == \old(this.p2); */
  /**@ ensures \result.latitude == this.p2.latitude; */
  /**@ ensures \result.latitude == \old(this.p2.latitude); */
  /**@ ensures \result.longitude == this.p2.longitude; */
  /**@ ensures \result.longitude == \old(this.p2.longitude); */
  /** @return this.p2 */
  public GeoPoint p2()
  {
    return this.p2;
  }

  /** @return this.length */
  public double length()
  {
    return p1.distanceTo(p2);
  }

  /**
   * @requires this.length != 0
   * @return this.heading
   */
  public double heading()
  {
    Assert.assert(length() != 0.0);
    return p1.headingTo(p2);
  }

  /**@ also_ensures \result != null; */
  public String toString()
  {
    return "Seg{" + name + "," + p1 + "," + p2 + "}";
  }

  /**
   * Compares the specified Object with with this GeoSegment for equality.
   * @return    gs != null && (gs instanceof GeoSegment)
   *         && gs.name = this.name && gs.p1 = this.p1 && gs.p2 = this.p2
   */
  public boolean equals(Object o)
  {
    if (!(o instanceof GeoSegment))
      return false;

    GeoSegment other = (GeoSegment) o;
    return
      this.p1.equals(other.p1) &&
      this.p2.equals(other.p2) &&
      this.name.equals(other.name);
  }

  // specified by superclass (Object)
  public int hashCode()
  {
    return
      name.hashCode() +
      p1.hashCode() * 7 +
      p2.hashCode() * 17;
  }

} // GeoSegment



