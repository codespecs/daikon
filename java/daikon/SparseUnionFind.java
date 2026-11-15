package daikon;

import java.util.HashMap;
import java.util.Map;

/**
 * A union-find (disjoint-set) data structure over all the ints, including negative ones. Initially,
 * each int is in its own set. The representative of each set is its smallest element.
 *
 * <p>Space is proportional to the number of elements that are not the representative of their set,
 * so the elements can be sparse, such as 3, 1207, and -1.
 */
final class SparseUnionFind {

  /**
   * Maps each element that is not the representative of its set to another element of the same set.
   * A representative has no entry.
   */
  private final Map<Integer, Integer> parent = new HashMap<>();

  /** Creates a new SparseUnionFind in which each int is in its own set. */
  SparseUnionFind() {}

  /**
   * Returns true if each int is in its own set, that is, if no call to {@link #union} has merged
   * two different sets.
   *
   * @return true if each int is in its own set
   */
  boolean isEmpty() {
    return parent.isEmpty();
  }

  /**
   * Returns the representative of the set that contains the given element. Compresses the path from
   * the element to the representative.
   *
   * @param x an element, such as 3
   * @return the representative of the set that contains {@code x}
   */
  int find(int x) {
    int root = x;
    Integer next;
    while ((next = parent.get(root)) != null) {
      root = next;
    }
    while (x != root) {
      next = parent.put(x, root);
      assert next != null : "@AssumeAssertion(nullness): x is not a representative";
      x = next;
    }
    return root;
  }

  /**
   * Merges the sets that contain the given elements. The representative of the merged set is the
   * smaller of the two representatives.
   *
   * @param x an element, such as 3
   * @param y another element, such as 5
   */
  void union(int x, int y) {
    int rootX = find(x);
    int rootY = find(y);
    if (rootX < rootY) {
      parent.put(rootY, rootX);
    } else if (rootY < rootX) {
      parent.put(rootX, rootY);
    }
  }
}
