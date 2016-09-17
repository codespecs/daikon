package MapQuick1;

import MapQuick.*;
import java.util.*;
import junit.framework.Assert;

/**
 * PathFinder is a procedural abstraction which searches Graphs for a
 * shortest path.  The search is uses a greedy algorithm, which means
 * that the search space must support dynamic programming.
 */
public class PathFinder
{

  /**
   * @param g the graph to be searched
   * @param starts start points, given by one-element paths
   * @param goals goal points, given by graph nodes
   *
   * @requires no argument is null
   * @requires starts.size >= 1
   * @requires goals.size >= 1
   * @requires for all s in starts, s instanceof Path
   * @requires the paths in starts will accept nodes of the type found in the graph
   * @requires for any node n in this graph, a path which ends with n
   *   may be successfully extended by n's children
   *
   * @returns the shortest path from any one start to any one goal
   *
   * @throws NoPathException if no path exists from any start to any goal
   *
   * @throws IllegalArgumentException if any start or goal node is not a node in the graph
   * @throws IllegalArgumentException if any start path does not have exactly one element
   */
  public static Path findPath(Graph g, Set starts, Set goals) throws NoPathException
  {
    Assert.assertNotNull(g);
    Assert.assertNotNull(starts);
    Assert.assertNotNull(goals);
    Assert.assertTrue(starts.size() >= 1);
    Assert.assertTrue(goals.size() >= 1);

    // check arguments
    {
      Iterator iter = starts.iterator();
      while (iter.hasNext()) {
	Path startpath = (Path) iter.next();
	Iterator elts = startpath.elements();
	if (!elts.hasNext()) throw new IllegalArgumentException(startpath + " has no elements");
	Object start = elts.next();
	if (elts.hasNext()) throw new IllegalArgumentException(startpath + " has more than one element");
	if (!g.containsNode(start)) throw new IllegalArgumentException(start + " is not in the graph");
      }
      iter = goals.iterator();
      while (iter.hasNext()) {
	Object goal = iter.next();
	if (!g.containsNode(goal)) throw new IllegalArgumentException(goal + " is not in the graph");
      }
    }


    // Map paths = { forall start in starts | (start, [start]) }
    // MapQuick.PriorityQueue active = starts

    Map paths = new LinkedHashMap();
    MapQuick.PriorityQueue active = new MapQuick.PriorityQueue();
    {
      Iterator iter = starts.iterator();
      while (iter.hasNext()) {
	Path startpath = (Path) iter.next();
	Object start = startpath.elements().next();
	Object old = paths.put(start, startpath);
	Assert.assertTrue(old == null);
	active.insert(startpath.cost(), start);
      }
    }

    // Set finished = { }

    Set finished = new LinkedHashSet();

    // while active is non-empty do

    while (active.size() > 0) {

      // y = active.extractMin()
      // ypath = paths(y)
      Object y = active.extractMin();
      Path ypath = (Path) paths.get(y);
      Assert.assertTrue(y != null);
      Assert.assertTrue(ypath != null);

      // if (y in goals)
      //   return ypath
      if (goals.contains(y)) {
	return ypath;
      }

      // for each child z of y
      Iterator zs = g.childrenOf(y).iterator();
      while (zs.hasNext()) {
	Object z = zs.next();

	// zpath = ypath + [z]
	Path zpath = ypath.extend(z);

	// if (z not in finished) and (z not in active)
	if (!finished.contains(z) && !active.contains(z)) {

	  // [ missing from algorithm: check for y != z ]
	  if (y.equals(z)) {
	    continue;
	  }

	  // paths(z) = zpath
	  // insert z in active
	  Object old = paths.put(z, zpath);
	  Assert.assertTrue(old == null);
	  active.insert(zpath.cost(), z);
	}

      }

      // insert y in finished
      finished.add(y);
    }

    // No Path Exists
    throw new NoPathException();
  }

  public static class NoPathException extends Exception
  {
    // Only need the default constructor
  }

}
