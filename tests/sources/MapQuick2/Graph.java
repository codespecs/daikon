package MapQuick2;

import MapQuick.*;
import java.util.*;
import junit.framework.Assert;

/**
 * Graph represents a mutable, directed graph.
 *
 * @specfield  nodes : set                      // the nodes in this Graph
 * @specfield  edges : binary relation on nodes // the pairs of adjacent nodes in this Graph
 * @endspec
 * <p>
 *
 * The equality of the nodes stored in this graph, as determined by
 * their equals(Object) method, must not change while the node is in
 * the Graph.
 */
public class Graph
{

  /**
   * @effects Creates a new, empty Graph
   */
  public Graph()
  {
    checkRep();
  }

  // Object -> Set[Object]
  private final LinkedHashMap adjMap = new LinkedHashMap();

  public void checkRep()
  {
    Iterator nodes = adjMap.keySet().iterator();
    while (nodes.hasNext()) {
      Object node = nodes.next();
      Assert.assertNotNull(node);
      Iterator children = ((Set) adjMap.get(node)).iterator();
      while (children.hasNext()) {
	Object child = children.next();
	Assert.assertTrue(adjMap.containsKey(child));
      }
    }
  }

  /**
   * @requires node != null
   * @modifies this.nodes
   * @effects adds the node to this.nodes
   * @throws DuplicateNodeException if node is already in in this.nodes
   */
  public void addNode(Object node)
    throws DuplicateNodeException
  {
    Assert.assertNotNull(node);
    //checkRep();

    if (adjMap.containsKey(node))
      throw new DuplicateNodeException(node.toString());

    adjMap.put(node, new HashSet(4));

    //checkRep();
  }

  /**
   * @requires node != null
   * @returns true iff node in this.nodes
   */
  public boolean containsNode(Object node)
  {
    return adjMap.containsKey(node);
  }

  /**
   * @requires node != null
   * @returns an immutable view of the nodes in this.
   * Changes which are made to the graph are reflected in this view.
   */
  public Set nodeSet()
  {
    return Collections.unmodifiableSet(adjMap.keySet());
  }

  /**
   * @requires node != null
   * @returns a reference to the adjacency list for the given node
   *          (which then may be mutated directly)
   * @throws NoNodeException if node not in this.nodes
   */
  private Set adjTo(Object node)
  {
    Assert.assertNotNull(node);
    Set result = (Set) adjMap.get(node);
    if (result == null) throw new NoNodeException(node.toString());
    return result;
  }

  /**
   * @requires from, to != null
   * @modifies this.edges
   * @effects adds an edge from "from" to "to"
   * @throws NoNodeException if from or to not in this.nodes
   * @throws DuplicateEdgeException if (from, to) already in this.edges
   */
  public void addEdge(Object from, Object to)
    throws NoNodeException, DuplicateEdgeException
  {
    Assert.assertNotNull(from);
    Assert.assertNotNull(to);
    //checkRep();

    Set adj = adjTo(from);
    if (!this.containsNode(to))
      throw new NoNodeException(to.toString());

    if (adj.contains(to))
      throw new DuplicateEdgeException("(" + from + "," + to + ")");

    adj.add(to);

    //checkRep();
  }

  /**
   * @requires node != null
   * @returns an immutable view of the children of the given node.
   * Changes which are made to the graph are reflected in this view.
   * @thows NoNodeException if node not in this.nodes
   */
  public Collection childrenOf(Object node)
  {
    Assert.assertNotNull(node);
    //checkRep();

    Set adj = adjTo(node);
    Collection result = Collections.unmodifiableCollection(adj);

    //checkRep();
    return result;
  }

  // ==================== Exceptions ====================

  public abstract class GraphException
    extends RuntimeException
  {
    public GraphException() { }
    public GraphException(String s) { super(s); }
    /** @return the Graph which caused this exception */
    public Graph getGraph() { return Graph.this; }
  }

  public class DuplicateNodeException
    extends GraphException
  {
    public DuplicateNodeException() { }
    public DuplicateNodeException(String s) { super(s); }
  }

  public class NoNodeException
    extends GraphException
  {
    public NoNodeException() { }
    public NoNodeException(String s) { super(s); }
  }

  public class DuplicateEdgeException
    extends GraphException
  {
    public DuplicateEdgeException() { }
    public DuplicateEdgeException(String s) { super(s); }
  }
}
