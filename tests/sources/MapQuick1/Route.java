package MapQuick1;

import MapQuick.*;
import junit.framework.Assert;

/**
 * A Route models a route on the earth from one location to another.
 * Routes are immutable.<p>
 *
 * @specfield start : GeoPoint            // location of the start of the route
 * @specfield end : GeoPoint              // location of the end of the route
 * @specfield startHeading : double       // direction of travel at the start of the route, in degrees
 * @specfield endHeading : double         // direction of travel at the end of the route, in degrees
 * @specfield length : double             // total length of the route, in miles
 * @endspec
 *
 * <p> Although a Route may be formed from a sequence of GeoSegments,
 * Route does not provide a way to retrieve the segments used in such
 * a construction.<p>
 *
 * While Routes are immutable, new Routes can be constructed by adding a
 * segment to the end of a Route.  An added segment must be properly
 * oriented; that is, its p1 field must correspond to the end of the
 * original Route, and its p2 field corresponds to the end of the new
 * Route.<p>
 *
 * Because a Route is not necessarily straight, its length -- the
 * distance traveled by following the path from start to end -- is not
 * necessarily the same as the distance along a straight line between its
 * endpoints.<p>
 */
public abstract class Route {

  // Constructors

  /** Constructs a Route consisting of a single GeoSegment.
   * @requires gs != null
   * @effects
   * <pre>
   *      this.start = gs.p1
   *   && this.end = gs.p2
   *   && this.startHeading = gs.heading
   *   && this.endHeading = gs.heading
   *   && this.length = gs.length
   * </pre>
   */
  public Route(GeoSegment gs)
  {
    first = gs;
    last = gs;
    length = gs.length();
  }

  protected final GeoSegment first;
  protected final GeoSegment last;
  private final double length;

  protected Route(Route r, GeoSegment gs)
  {
    Assert.assertNotNull(r);
    Assert.assertNotNull(gs);

    first = r.first;
    last = gs;
    length = r.length + gs.length();
  }
  
  // Observers

  /** @return this.start */
  public GeoPoint start()
  {
    return first.p1();
  }

  /** @return this.end */
  public GeoPoint end()
  {
    return last.p2();
  }

  /** @return this.startHeading */
  public double startHeading()
  {
    return first.heading();
  }

  /** @return this.endHeading */
  public double endHeading()
  {
    return last.heading();
  }

  /** @return this.length */
  public double length()
  {
    return length;
  }

  /**
   * Give directions for following this Route, starting at its
   * start point and facing in the specified heading.
   * @requires 0 <= heading < 360
   * @return a String giving human-readable directions from start to
   * end along this route. The return value is the concatenation
   * of e.directions() for e in elementaryRoutes(). Each line of
   * the directions is of the form described in
   * ElementaryRoute.directions(double heading).
   * @see ElementaryRoute#directions(double heading)
   */
  public abstract String directions(double heading);


  /**
   * Returns a sequence of ElementaryRoute objects.
   * The concatenation of the ElementaryRoutes, in order, is equivalent to
   * this route.  No two consecutive ElementaryRoute objects have the same
   * name.<p>
   *
   * @return a new array, a, of ElementaryRoutes such that
   * <pre>
   *      this.start        = a[0].start
   *   && this.startHeading = a[0].startHeading
   *   && this.end          = a[ a.length - 1 ].end
   *   && this.endHeading   = a[ a.length - 1 ].endHeading
   *   && this.length       =  sum (0 <= i < a.length) . a[i].length
   *   && for all integers i .
   *          ( 0 <= i < a.length - 1 => ( a[i].name != a[i+1].name &&
   *                                       a[i].end   = a[i+1].start ) )
   * </pre>
   */
  public abstract ElementaryRoute[] elementaryRoutes();

  /**
   * Compares the specified Object with this Route for equality.
   * @return true iff o has the same runtime class as this and all of
   * o's specification fields are value equivalent to the specation fields
   * of this.
   */
  public boolean equals(Object r)
  {
    return (r instanceof Route) &&
      equals((Route) r);
  }

  public boolean equals(Route r)
  {
    return (r != null) &&
      (this.first.equals(r.first)) &&
      (this.last.equals(r.last));
  }

  // Specified by Object superclass
  public int hashCode()
  {
    return first.hashCode() + last.hashCode() * 7;
  }

  // Specified by Object superclass
  public String toString()
  {
    return "{Route;first=" + first + ";last=" + last + "}";
  }


} // Route



