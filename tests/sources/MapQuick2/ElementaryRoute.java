package MapQuick2;

import MapQuick.*;
import java.util.*;
import junit.framework.Assert;

/**
 * An ElementaryRoute represents a route from one location to another along
 * a single geographic feature.
 * ElementaryRoutes are immutable.<p>
 *
 * ElementaryRoute abstracts over a sequence
 * of GeoSegments, all of which have the same name, thus providing a
 * representation for nonlinear or nonatomic geographic features.
 * As an example, an ElementaryRoute might represent the course of a
 * winding river, or travel along a road through intersections but
 * remaining on the same road.<p>
 *
 * ElementaryRoute adds the following specification field to Route:
 * @specfield  name : String
 * @endspec
 * <p>
 * <b>Rationale</b>:
 *
 * Since GeoSegments are linear, and because even straight courses may be
 * split into multiple GeoSegments for other reasons (such as representing
 * intersections), some features cannot be represented by a single
 * GeoSegment.  However, when labeling rivers on a map, giving driving
 * directions along roads, etc., multiple segments along the same feature
 * should be treated as a unit.  For instance, it would not be useful for
 * directions to say,
 *   <pre>
 *   Turn right onto Lombard Street and go 0.1 miles.
 *   Turn left onto Lombard Street and go 0.1 miles.
 *   Turn right onto Lombard Street and go 0.1 miles.
 *   Turn left onto Lombard Street and go 0.1 miles.
 *   Turn right onto Lombard Street and go 0.1 miles.
 *   Turn left onto Lombard Street and go 0.1 miles.
 *   Turn right onto Lombard Street and go 0.1 miles.</pre>
 * Instead, the directions should say,
 *   <pre>
 *   Turn right onto Lombard Street and go 0.7 miles.</pre>
 *
 * As another example, Mass Ave might be represented by one segment from
 * the Harvard Bridge to Vassar Street, another from Vassar Street to Main
 * Street, and another from Main Street to Prospect Street.  The directions
 * should not say
 *   <pre>
 *   Turn right onto Mass Ave and go 0.2 miles.
 *   Continue onto Mass Ave and go 0.4 miles.
 *   Continue onto Mass Ave and go 0.2 miles.</pre>
 * Instead, the directions should say,
 *   <pre>
 *   Turn right onto Mass Ave and go 0.8 miles.</pre>
 * <p>
 */

public class ElementaryRoute extends Route {

  // fields
  private String name;
  private final static double REP_SCALE_FACTOR = 1000000.0;

  // Constructors

  /**
   * Constructs a new ElementaryRoute.
   * @effects Constructs a new ElementaryRoute, r, such that
   *	      r.name = gs.name.  The specification fields of r inherited
   *	      from Route are set according to the specification of
   *	      Route(GeoSegment gs).
   * @see Route#Route(GeoSegment gs)
   */
  public ElementaryRoute(GeoSegment gs){
    super(gs);
    this.name = gs.name();
  }

  // Constructs a Route consisting of a GeoSegment gs added to the Route r
  // requires: gs != null && el != null && gs.p1 = r.end && gs.name = r.name
  // effects:
  //           this.start = r.start
  //           this.end = gs.p2
  //           this.length = r.length + gs.length
  //           this.startHeading = r.startHeading
  //           this.endHeading = gs.heading
  private ElementaryRoute(ElementaryRoute el,GeoSegment gs){
    super(el,gs);
    Assert.assertTrue(gs.name().equals(el.name()));
    this.name = el.name();
  }

  // Producers

  /**
   * Extends the route by adding a new segment to its end.  The segment must
   * be properly oriented, and its name must be the same as the name of
   * this ElementaryRoute.<p>
   *
   * @requires gs != null && gs.p1 = this.end && gs.name = this.name
   * @return a new ElementaryRoute r such that
   *       r.end = gs.p2
   *    && r.endHeading = gs.heading
   *    && r.length = this.length + gs.length
   */
  public ElementaryRoute addSegment(GeoSegment gs){
    // the assert is done in the constructor
    return new ElementaryRoute(this,gs);
  }

  // Observers

  // @return this.name
  public String name(){
    return this.name;
  }

  /**
   * Give directions for following this ElementaryRoute, starting at its
   * start point and facing in the specified heading.
   * @requires 0 <= heading < 360
   * @return a one-line newline-terminated human-readable directions string.
   * The directions string is of the form
   *     <pre>
   *     Turn right onto Baker Street and go 1.2 miles.
   *     </pre>
   * Here, "Turn right" rotates the traveler from the given heading to
   * this.startHeading, "Baker Street" is this.name, and 1.2 miles is
   * this.length.  The length is printed with tenth-of-a-mile precision.<p>
   *
   * Let the turn angle be a.  The tur  n should be annotated as
   * <pre>
   *   Continue          if a < 10
   *   Turn slight right if 10 <= a < 60
   *   Turn right        if 60 <= a < 120
   *   Turn sharp right  if 120 <= a < 179
   *   U-turn            if 179 <= a
   * </pre>
   * and likewise for left turns.<p>
   */
  public String directions(double heading){

    Assert.assertTrue((0<= heading) && (heading<360));

    // calculate both the initial angle
    boolean clockwise = false;
    double initial = this.startHeading() - heading;
    double angle;

    if (initial >= 0)
      clockwise = true;

    if(Math.abs(initial)<180){
      angle = initial;
    }
    else{
      angle= 360 - Math.abs(initial);
      if(clockwise)
	angle = -angle;
    }
    String direction;
    // if angle is positive, deal with the right case
    if(angle>=0){
      if(angle<10)
	direction = "Continue";
      else if(angle <60)
	direction = "Turn slight right";
      else if(angle<120)
	direction = "Turn right";
      else if(angle<179)
	direction = "Turn sharp right";
      else
	direction ="U-turn";
    } else {
    //if angle is negative, deal with the left case
      if(angle>-10)
	direction = "Continue";
      else if(angle>-60)
	direction = "Turn slight left";
      else if(angle>-120)
	direction = "Turn left";
      else if(angle>-179)
	direction = "Turn sharp left";
      else
	direction ="U-turn";
    }
    //rounding business, is there something more efficient?
    double l = this.length() * 10;
    l = Math.round(l);
    l = l /10;
    return direction+" onto "+this.name()+" and go "+l+" miles.\n";
  }

  // return an array of ElementaryRoute[] with the only element <this>
  public ElementaryRoute[] elementaryRoutes(){
    ElementaryRoute[] erArray = new ElementaryRoute[1];
    erArray[0] = this;
    return erArray;
  }

  /**
   * Compares the specified Object with with this ElementaryRoute for equality.
   * @return    r != null && (r instanceof ElementaryRoute)
   *         && r.name = this.name && r.start = this.start && r.end = this.end
   *         && r.startHeading = this.startHeading
   *         && r.endHeading = this.endHeading && r.length = this.length
   */

  public boolean equals(Object er){
    Assert.assertTrue(er != null);
    Assert.assertTrue(er instanceof ElementaryRoute);

    if((er != null) && (er instanceof ElementaryRoute)){
      ElementaryRoute r = (ElementaryRoute)er;
      return super.equals(r) && (this.name() == r.name());
    }else
      return false;
  }


  /**
   * @return a valid hashcode for this.
   */
  public int hashCode() {
    // 2 ElementaryRoutes are equal if they have the same name, the same start,
    // the same end, the same startHeading, the same endHeading and the same
    // length
    return super.hashCode() + name.hashCode();
  }

  /**
   * @return a string representation of this.
   */
  public String toString(){
    return "Elementary"+super.toString()+ this.name()+ "}";
  }

} // ElementaryRoute
