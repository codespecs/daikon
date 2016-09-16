package MapQuick;

import java.util.*;

/**
 * A WeightedNodePath characterizes a path of WeightedNodes.  The cost
 * for a path is the sum of the costs of the WeightedNodes it contains.
 */

public class WeightedNodePath implements Path {

  //
  // RepInvariant:
  //   (node != null) &&
  //   (path == null) ==> (cost == node.cost) &&
  //   (path != null) ==> (cost == node.cost + path.cost)
  //
  
  private final WeightedNode node;
  private final WeightedNodePath path;
  private final int cost;
  

  /**
   * Constructs a WeightedNodePath containing one node.
   * 
   * @requires node != null
   * @effects Creates a new WeightedNodePath which originates at
   * <code>node</code>.
   */
  public WeightedNodePath(WeightedNode node) {
    this(node, null);
  }

  /**
   * @requires node != null
   * @effects Creates a new WeightedNodePath 'res' such that
   * res.elements = path.elements + [ node ]
   */
  private WeightedNodePath(WeightedNode node, WeightedNodePath path) {
    if (node == null) {
      throw new IllegalArgumentException();
    }
    this.node = node;
    this.path = path;
    if (path != null) {
      this.cost = node.cost + path.cost;
    } else {
      this.cost = node.cost;
    }
  }

  // Specified by the Path interface
  public Path extend(Object o) {
    if (o instanceof WeightedNode) {
      return extend((WeightedNode)o);
    }
    throw new IllegalArgumentException();
  }

  // Specified by the Path interface
  public Path extend(WeightedNode node) {
    return new WeightedNodePath(node, this);
  }

  // Specified by the Path interface
  public double cost() {
    return cost;
  }

  // Specified by the Path interface
  public Iterator elements() {
    LinkedList l = new LinkedList();
    WeightedNodePath wnp = this;
    while (wnp != null) {
      l.addFirst(wnp.node);
      wnp = wnp.path;
    }
    return Collections.unmodifiableList(l).iterator();
  }

  // Specified by the Object superclass
  public String toString() {
    StringBuffer buff = new StringBuffer();
    buff.append("[WeightedNodePath: ");
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
   * @return true iff o is a WeightedNodePath and o.elements is the
   * same sequence as this.elements
   */
  public boolean equals(Object o) {
    if (o instanceof WeightedNodePath) {
      return this.equals((WeightedNodePath) o);
    } else {
      return false;
    }
  }

  /**
   * @return true iff wnp.elements is the same sequence as this.elements
   */
  public boolean equals(WeightedNodePath wnp) {
    return (wnp != null) && this.node.equals(wnp.node) && (this.path == null ? wnp.path==null : this.path.equals(wnp.path));
    // Java 7 and Java 8 assign different line numbers
    // to multi-line return statements.
  }

  // Specified by the Object superclass
  public int hashCode() {
    return node.hashCode() + (this.path==null ? 0 : 13 * path.hashCode());
  }					       
}    

