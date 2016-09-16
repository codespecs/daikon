package MapQuick2;

import MapQuick.*;
import java.util.*;
import junit.framework.*;

/**
 * A CompositeRoute is a Route that can traverse arbitrary GeoSegments,
 * regardless of their names.<p>
 * @specfield elemRoutes : sequence   // ElementaryRoute objects, which combined make this Route
 * @endspec
 * <p>
 *
 * Note that CompositeRoute inherits specification fields <b>start</b>,
 * <b>end</b>, <b>startHeading</b>, <b>endHeading</b>, and <b>length</b>
 * from Route<p>.
 *
 * CompositeRoutes are immutable.<p>
 */

public class CompositeRoute
  extends Route
{

  // private fields
  private Cons elements = null;

  // Constructors

  /**
   * @effects Constructs a new CompositeRoute.
   * @see Route#Route(GeoSegment gs)
   */
  public CompositeRoute(GeoSegment gs){
    //assert in the constructor
    super(gs);
    //add the elementaryRoute made to this.elements
    this.elements = new Cons(new ElementaryRoute(gs), null);
  }

  // constructor
  // requires: gs != null && gs.p1 = this.end
  // effects: Constructs a Route consisting of a GeoSegment gs
  //          added to the Route r
  //
  //           this.start = r.start
  //           this.end = gs.p2
  //           this.length = r.length + gs.length
  //           this.startHeading = r.startHeading
  //           this.endHeading = gs.heading
  //           this.elements.head = gs, this.elements.tail = cr.elements

  private CompositeRoute(CompositeRoute cr,GeoSegment gs){
    //assert in the constructor
    super(cr,gs);

    // If the name matches, extend head and use same tail
    // Else, make a new head and set tail to previous route
    if ((cr.elements != null) &&
        (cr.elements.head.name().equals(gs.name()))) {
      ElementaryRoute newHead = cr.elements.head.addSegment(gs);
      this.elements = new Cons(newHead, cr.elements.tail);
    } else {
      this.elements = new Cons(new ElementaryRoute(gs), cr.elements);
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
  public CompositeRoute addSegment(GeoSegment gs){
    return new CompositeRoute(this,gs);
  }

  /** return the array of elementaryRoutes of <this>, in order
   */
  public ElementaryRoute[] elementaryRoutes() {

    LinkedList routes = new LinkedList();

    Cons index = elements;
    while (index != null) {
      // store the head as the first thing in the result, since we are
      // walking from the end of the route to the beginning
      routes.addFirst(index.head);
      index = index.tail;
    }

    // put the List into an array
    ElementaryRoute[] result = new ElementaryRoute[routes.size()];
    result = (ElementaryRoute[]) routes.toArray(result);

    return result;
  }


  // return directions of <this>
  public String directions(double heading){
    ElementaryRoute[] routes = this.elementaryRoutes();
    double initialOrientation = heading;
    String output = "";
    for(int i=0; i<routes.length; i++){
      output = output + routes[i].directions(initialOrientation);
      initialOrientation = routes[i].endHeading();
    }
    return output;
  }


  /**
   * Compares the specified Object with this CompositeRoute for equality.
   * @return true iff (o instanceof CompositeRoute) && (o.elemRoutes and
   * this.elemRoutes contain the same elements in the same order).
   */
  public boolean equals(Object o){
    Assert.assertTrue(o != null);
    Assert.assertTrue(o instanceof CompositeRoute);

    if((o != null) && (o instanceof CompositeRoute)){
      CompositeRoute r = (CompositeRoute)o;
      ElementaryRoute[] me = elementaryRoutes();
      ElementaryRoute[] it = r.elementaryRoutes();

      if (me.length != it.length){
	return false;
      }else {

	for (int i=0; i<me.length; i++){
	  if(!me[i].equals(it[i]))
	    return false;
	}
	return super.equals(r);
      }
    }
    return false;
  }
  /**
   * @return a valid hash code for this.
   */
  public int hashCode() {
    // This implementation will work, but you may want to modify it later
    // for improved performance.  If you do change the implementation, make
    // sure it holds the hashCode invariant.  That is, if equals returns
    // true for two objects, then they must have the same hashCode.
    return super.hashCode() + elementaryRoutes().hashCode();
  }

  /**
   * @return a string representation of this.
   */
  public String toString(){
    String routeString = "";

    ElementaryRoute[] routes = this.elementaryRoutes();
    for(int i = 0; i< routes.length; i++)
      routeString = routeString + routes[i].toString() + " ";
    return "Composite"+super.toString()+ "< " + routeString + " > }";
  }



  /** Cons is a simple cons cell record type. */
  static class Cons {
    final ElementaryRoute head;
    final Cons tail;
    Cons(ElementaryRoute h, Cons t) { head = h; tail = t; }
  }

}
