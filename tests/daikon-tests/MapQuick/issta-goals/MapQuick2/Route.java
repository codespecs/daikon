package MapQuick2;

import MapQuick.*;
import junit.framework.Assert;

/**
 * A Route models a route on the earth from one location to another.<p>
 *
 * @specfield start : GeoPoint            // location that starts the route
 * @specfield end : GeoPoint              // location of the end of the route
 * @specfield startHeading : double       // direction of travel at the start of the route, in degrees
 * @specfield endHeading : double         // direction of travel at the end of the route, in degrees
 * @specfield length : double             // total length of the route, in miles
 * @endspec
 * <p>
 * Routes are immutable.<p>
 *
 * Although a Route may be constructed from a sequence of GeoSegments,
 * Route does not provide a way to retrieve the segments used in such
 * a construction.  While Routes are immutable, new Routes can be
 * constructed by adding a segment to the end of a Route.  An added segment
 * must be properly oriented; that is, its p1 field must correspond to the
 * end of the route.<p>
 *
 * Because a Route is not necessarily straight, its length -- the
 * distance traveled by following the path from start to end -- is not
 * necessarily the same as the distance along a straight line between its
 * endpoints.<p>
 */
public abstract class Route {

  /*@ invariant this.start != null; */
  /*@ invariant this.end != null; */
  // fields
  /*@ spec_public */ private GeoPoint start;
  /*@ spec_public */ private GeoPoint end;
  /*@ spec_public */ private double startHeading;
  /*@ spec_public */ private double endHeading;
  /*@ spec_public */ private double length;
  /*@ spec_public */ private final static double REP_SCALE_FACTOR = 1000000.0;

  /*@ requires gs != null; */
  /**@ requires gs.name != null; */
  /**@ requires gs.p1 != null; */
  /**@ requires gs.p2 != null; */
  /*@ ensures gs.p1 == this.start; */
  /*@ ensures gs.p2 == this.end; */
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
  public Route(GeoSegment gs){
    Assert.assertNotNull(gs);
    this.start = gs.p1();
    this.end = gs.p2();
    this.startHeading = gs.heading();
    this.endHeading = gs.heading();
    this.length = gs.length();
  }

  /*@ requires r != null; */
  /*@ requires gs != null; */
  /**@ requires gs.name != null; */
  /**@ requires gs.p1 != null; */
  /**@ requires gs.p2 != null; */
  /*@ modifies r.startHeading, r.endHeading, r.length; */
  /*@ ensures r.start == this.start; */
  /*@ ensures gs.p2 == this.end; */
  // constructor
  // requires: gs != null && r != null && gs.p1 = r.end
  // effects: Constructs a new Route consisting of a GeoSegment gs
  //          added to the Route r
  //
  //           this.start = r.start
  //           this.end = gs.p2
  //           this.length = r.length + gs.length
  //           this.startHeading = r.startHeading
  //           this.endHeading = gs.heading
  protected Route(Route r,GeoSegment gs){
    Assert.assertNotNull(r);
    Assert.assertNotNull(gs);
    Assert.assert(gs.p1().equals(r.end()));
    this.start = r.start();
    this.end = gs.p2();
    this.startHeading = r.startHeading();
    this.endHeading = gs.heading();
    this.length = r.length() + gs.length();
  }

  /*@ modifies this.startHeading, this.endHeading, this.length; */
  /*@ ensures \result == this.start; */
  /**@ ensures \result == \old(this.start); */
  /**@ ensures \result.latitude == this.start.latitude; */
  /**@ ensures \result.latitude == \old(this.start.latitude); */
  /**@ ensures \result.longitude == this.start.longitude; */
  /**@ ensures \result.longitude == \old(this.start.longitude); */
  // Observers

  /** @return this.start */
  public GeoPoint start(){
    return this.start;
  }

  /*@ modifies this.startHeading, this.endHeading, this.length; */
  /*@ ensures \result == this.end; */
  /**@ ensures \result == \old(this.end); */
  /**@ ensures \result.latitude == this.end.latitude; */
  /**@ ensures \result.latitude == \old(this.end.latitude); */
  /**@ ensures \result.longitude == this.end.longitude; */
  /**@ ensures \result.longitude == \old(this.end.longitude); */
  /** @return this.end */
  public GeoPoint end(){
    return this.end;
  }

  /*@ modifies this.startHeading, this.endHeading, this.length; */
  /** @return this.startHeading */
  public double startHeading(){
    return this.startHeading;
  }

  /*@ modifies this.startHeading, this.endHeading, this.length; */
  /** @return this.endHeading */
  public double endHeading(){
    return this.endHeading;
  }

  /*@ modifies this.startHeading, this.endHeading, this.length; */
  /** @return this.length */
  public double length(){
    return this.length;
  }

  /** Computes directions for following a route.
   * @requires 0 <= heading < 360
   * @return a String giving human-readable directions from start to end along this route.
   * The return value is the concatenation of e.directions() for e in
   * elementaryRoutes().
   * Each line of the directions is of the form described in
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
   *      ( 0 <= i < a.length - 1 => ( a[i].name != a[i+1].name &&
   *                                   a[i].end   = a[i+1].start ) )
   * </pre>
   */
  public abstract ElementaryRoute[] elementaryRoutes();

  /**
   * Compares the specified Object with this Route for equality.
   * @return true iff o has the same runtime class as this and all of
   * o's specification fields are value equivalent to the specation fields
   * of this.
   */
  public boolean equals(Object o) {
    if (o instanceof Route) {
      Route r = (Route)o;
      return ((start.equals(r.start())) && (end.equals(r.end())) && (startHeading == r.startHeading()) && (endHeading == r.endHeading()) && (length == r.length()));
    }
    else
      return false;
  }

  /**
   * @return a valid hashcode for this.
   */
  public int hashCode() {
    // This implementation will work, but you may want to modify it later
    // for improved performance.  If you do change the implementation, make
    // sure it holds the hashCode invariant.  That is, if equals returns
    // true for two objects, then they must have the same hashCode.
    return (2*start.hashCode())+(7*(int)(startHeading*REP_SCALE_FACTOR))+(3*end.hashCode())+(11*(int)(endHeading*REP_SCALE_FACTOR))+17*(int)(length*REP_SCALE_FACTOR);
  }

  /**
   * @return a string representation of this.
   */
  public String toString(){

    return "Route{" + start + "," + end + "," + startHeading + "->" + endHeading + "," + length;
  }



} // Route


