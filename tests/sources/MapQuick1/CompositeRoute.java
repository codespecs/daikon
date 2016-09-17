package MapQuick1;

import MapQuick.*;
import java.util.*;
import junit.framework.Assert;

/**
 * A CompositeRoute is a Route that can traverse arbitrary GeoSegments,
 * regardless of their names.
 * CompositeRoutes are immutable.<p>
 *
 * @specfield elemRoutes : sequence   // ElementaryRoute objects, which combined make this Route
 * @endspec
 * <p>
 *
 * Note that CompositeRoute inherits specification fields <b>start</b>,
 * <b>end</b>, <b>startHeading</b>, <b>endHeading</b>, and <b>length</b>
 * from Route.<p>
 */
public class CompositeRoute extends Route
{

  private final ElementaryRoute car;
  private final CompositeRoute cdr;

  /**
   * @requires gs != null
   * @effects Constructs a new CompositeRoute.
   * @see Route#Route(GeoSegment gs)
   */
  public CompositeRoute(GeoSegment gs)
  {
    super(gs);
    car = new ElementaryRoute(gs);
    cdr = null;
  }

  private CompositeRoute(CompositeRoute r, GeoSegment gs)
  {
    super(r, gs);
    Assert.assertTrue(r.end().equals(gs.p1()));

    if (r.car.name().equals(gs.name())) {
      car = r.car.addSegment(gs);
      cdr = r.cdr;
    } else {
      car = new ElementaryRoute(gs);
      cdr = r;
    }
  }

  // Producers

  /**
   * Extends the route by adding a new segment to its end.
   * @requires gs != null && gs.p1 = this.end
   * @return a new CompositeRoute r such that
   *       r.end = gs.p2
   *    && r.endHeading = gs.heading
   *    && r.length = this.length + gs.length
   */
  public CompositeRoute addSegment(GeoSegment gs)
  {
    return new CompositeRoute(this, gs);
  }

  /**
   * Compares the specified Object with this CompositeRoute for equality.
   * @return true iff (o instanceof CompositeRoute) && (o.elemRoutes and
   * this.elemRoutes contain the same elements in the same order).
   */
  public boolean equals(Object o)
  {
    return (o instanceof CompositeRoute) &&
      equals((CompositeRoute) o);
  }

  public boolean equals(CompositeRoute other)
  {
    return super.equals(other) &&
      elementaryRoutesList().equals(other.elementaryRoutesList());
  }

  // Specified by Object superclass
  public String toString()
  {
    return "{Comp{" + super.toString()+ "}}";
  }

  // Specified by Route superclass
  public ElementaryRoute[] elementaryRoutes()
  {
    LinkedList routes = elementaryRoutesList();
    ElementaryRoute[] result = new ElementaryRoute[routes.size()];
    result = (ElementaryRoute[]) routes.toArray(result);
    return result;
  }

  private LinkedList elementaryRoutesList()
  {
    LinkedList result = new LinkedList();
    CompositeRoute walker = this;
    while (walker != null) {
      result.addFirst(walker.car);
      walker = walker.cdr;
    }
    return result;
  }

  public String directions(double heading)
  {
    Iterator routes = elementaryRoutesList().iterator();
    StringBuffer result = new StringBuffer();
    while (routes.hasNext()) {
      ElementaryRoute route = (ElementaryRoute) routes.next();
      result.append(route.directions(heading));
      heading = route.endHeading();
    }
    return result.toString();
  }

}
