package MapQuick1;

import MapQuick.*;
import java.util.*;
import junit.framework.Assert;

/**
 * TODO
 */
public class CompositeRoutePath implements Path
{

  private final GeoSegment node;
  private final CompositeRoutePath path;
  private final Set goals;
  private final double astar_cost;
  private final double route_cost;

  /**
   * Constructs a path containing one node.
   *
   * @requires node != null, |goals| >= 1, elements of goals are
   * GeoSegments
   *
   * @effects Creates a new path which originates at node, and uses
   * the A* metric towards the given goals
   */
  public CompositeRoutePath(GeoSegment node, Set goals)
  {
    this(node, new LinkedHashSet(goals), null);
  }

  /**
   * @requires node != null, goals is owned by this, |goals| >= 1,
   * elements of goals are GeoSegments
   *
   * @effects Creates a new CompositeRoutePath 'res' such that
   * res.elements = path.elements + [ node ]
   */
  private CompositeRoutePath(GeoSegment node, Set goals, CompositeRoutePath path)
  {
    Assert.assertTrue(node != null);
    Assert.assertTrue(goals != null);
    Assert.assertTrue(goals.size() >= 1);
    Assert.assertTrue((path == null) || (path.node.p2().equals(node.p1())));

    this.node = node;
    this.path = path;
    this.goals = goals;

    if (path != null) {
      this.route_cost = node.length() + path.route_cost;
    } else {
      this.route_cost = node.length();
    }

    // find the nearest goal
    double nearest_goal = Double.POSITIVE_INFINITY;
    for (Iterator iter = goals.iterator(); iter.hasNext(); ) {
      GeoSegment goal = (GeoSegment) iter.next();
      double dist = node.p2().distanceTo(goal.p2());
      if (dist < nearest_goal) {
	nearest_goal = dist;
      }
    }

    astar_cost = route_cost + nearest_goal;

    checkRep();
  }

  public void checkRep()
  {
    Assert.assertNotNull(node);

    // goals are geosegments
    Assert.assertNotNull(goals);
    Assert.assertTrue(goals.size() >= 1);
    for (Iterator iter = goals.iterator(); iter.hasNext(); ) {
      Assert.assertTrue(iter.next() instanceof GeoSegment);
    }

    // cost is increasing
    if (path != null) {
      Assert.assertTrue(astar_cost >= path.astar_cost);
      Assert.assertTrue(route_cost >= path.route_cost);
    }

    // elements are correctly aligned
    Iterator elements = elements();
    GeoSegment prev = (GeoSegment) elements.next();
    while (elements.hasNext()) {
      GeoSegment cur = (GeoSegment) elements.next();
      Assert.assertTrue(prev.p2().equals(cur.p1()));
      prev = cur;
    }
  }

  // Specified by the Path interface
  public Path extend(Object o)
  {
    Assert.assertTrue(o instanceof GeoSegment);
    return extend((GeoSegment) o);
  }

  // Specified by the Path interface
  public Path extend(GeoSegment node)
  {
    return new CompositeRoutePath(node, goals, this);
  }

  // Specified by the Path interface
  public double cost()
  {
    checkRep();
    return astar_cost;
  }

  // Specified by the Path interface
  public Iterator elements()
  {
    // don't call checkRep since checkRep calls me

    LinkedList result = new LinkedList();
    CompositeRoutePath walker = this;
    while (walker != null) {
      result.addFirst(walker.node);
      walker = walker.path;
    }

    // don't call checkRep since checkRep calls me
    return Collections.unmodifiableList(result).iterator();
  }

  /**
   * @returns a new Route which is equivalent to this path
   */
  public Route route()
  {
    checkRep();

    Iterator elements = elements();
    GeoSegment first = (GeoSegment) elements.next();
    CompositeRoute result = new CompositeRoute(first);
    while (elements.hasNext()) {
      GeoSegment seg = (GeoSegment) elements.next();
      result = result.addSegment(seg);
    }

    checkRep();
    return result;
  }

  // Specified by the Object superclass
  public String toString()
  {
    StringBuffer buff = new StringBuffer();
    buff.append("[CompositeRoutePath: ");
    Iterator i = elements();
    while (i.hasNext()) {
      buff.append(i.next());
      if (i.hasNext()) {
	buff.append(", ");
      }
    }
    buff.append("]");
    return buff.toString();
  }

  /**
   * @return true iff o is a CompositeRoutePath and o.elements is the
   * same sequence as this.elements
   */
  public boolean equals(Object o)
  {
    return (o instanceof CompositeRoutePath) && equals((CompositeRoutePath) o);
  }

  /**
   * @return true iff other.elements is the same sequence as this.elements
   */
  public boolean equals(CompositeRoutePath other)
  {
    return (other != null) &&
      this.node.equals(other.node) &&
      (this.path == null ? other.path == null : this.path.equals(other.path));
  }

  // Specified by the Object superclass
  public int hashCode()
  {
    return node.hashCode() + (this.path == null ? 0 : 13 * path.hashCode());
  }
}
