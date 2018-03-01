package daikon.asm;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/*>>>
import org.checkerframework.checker.interning.qual.*;
*/

/**
 * Implements a partition of distinct elements into disjoint sets. Each set in the partition is
 * represented as a tree.
 *
 * <p>The operation add(String t) add a new element t to the partition.
 *
 * <p>The operation union(String t1, T t2) unions the (possibly distinct) sets containing t1 and t2.
 *
 * <p>The operation getSets() returns the sets of the partition.
 *
 * <p>This implementation has the additional requirement that all new elements added to the
 * partition must be non-equal, where equality is determined according to the equals/hashCode
 * methods. In other words, if at some point the operation add(t1) is performed for some t1, and
 * t1.equals(t2), then attempting to perform add(t2) will result in an IllegalArgumentException.
 */
public class DSForest {

  // Maps the elements of the partition to their corresponding nodes.
  HashMap<String, DSForestNode> elements = new LinkedHashMap<String, DSForestNode>();

  // Counter for node unique ids.
  private static int idCounter = 0;

  // A node in the tree.
  /*@UsesObjectEquals*/
  private static class DSForestNode {
    public String element;
    public DSForestNode parent;
    public int rank;
    public int id;

    @SuppressWarnings({
      "initialization.invalid.field.write.unknown",
      "assignment.type.incompatible"
    }) // weakness of FBC type system
    public DSForestNode(String element) {
      this.element = element;
      this.parent = this;
      this.rank = 0;
      this.id = idCounter;
      idCounter++;
    }
  }

  /**
   * If e != null and is not already in the forest, adds e to the forest. Otherwise throws an
   * IllegalArgumentException.
   */
  public void add(String e) {
    if (e == null) throw new IllegalArgumentException("Element cannot be null.");
    if (elements.containsKey(e)) {
      throw new IllegalArgumentException("Element already in disjoin-set forest.");
    }

    // Make a new singleton set containing e.
    DSForestNode n = new DSForestNode(e);
    elements.put(e, n);
  }

  private DSForestNode find(DSForestNode n) {
    if (n.parent == n) {
      return n;
    } else {
      n.parent = find(n.parent);
      return n.parent;
    }
  }

  /**
   * x and y must be elements of the forest. If e1.equals(e2), or e1 and e2 are already in the same
   * set, has no effect. Otherwise, unions the sets containing e1 and e2.
   */
  public void union(String x, String y) {
    DSForestNode xNode = elements.get(x);
    if (xNode == null) throw new IllegalArgumentException();
    DSForestNode yNode = elements.get(y);
    if (yNode == null) throw new IllegalArgumentException();
    DSForestNode xRoot = find(xNode);
    DSForestNode yRoot = find(yNode);
    @SuppressWarnings("interning")
    boolean sameRoot = (x == y ? xRoot == yRoot : true);
    assert sameRoot;
    if (xRoot == yRoot) return; // x and y already in the same set.
    if (xRoot.rank > yRoot.rank) {
      yRoot.parent = xRoot;
    } else if (xRoot.rank < yRoot.rank) {
      xRoot.parent = yRoot;
    } else {
      yRoot.parent = xRoot;
      xRoot.rank++;
    }
  }

  // Returns the current state of the forest. Each list represents
  // one disjoint set.
  public Set<Set<String>> getSets() {

    Map<Integer, Set<String>> sets = new LinkedHashMap<Integer, Set<String>>();
    Set<Set<String>> retval = new LinkedHashSet<Set<String>>();

    for (DSForestNode n : elements.values()) {
      int rootId = find(n).id;
      Set<String> set = sets.get(rootId);
      if (set == null) {
        set = new LinkedHashSet<String>();
        sets.put(rootId, set);
      }
      set.add(n.element);
    }

    for (Set<String> set : sets.values()) {
      retval.add(set);
    }

    return retval;
  }
}
