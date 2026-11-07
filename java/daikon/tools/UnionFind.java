package daikon.tools;

/** A union-find (disjoint-set) data structure over the integers 0..n-1. */
final class UnionFind {
  /** The parent of each element; an element is a root if it is its own parent. */
  private final int[] parent;

  /**
   * Creates a new UnionFind in which each element is in its own set.
   *
   * @param n the number of elements
   */
  UnionFind(int n) {
    parent = new int[n];
    for (int i = 0; i < n; i++) {
      parent[i] = i;
    }
  }

  /**
   * Returns the representative of the set containing the element.
   *
   * @param x an element
   * @return the representative of the set containing x
   */
  int find(int x) {
    while (parent[x] != x) {
      parent[x] = parent[parent[x]];
      x = parent[x];
    }
    return x;
  }

  /**
   * Merges the sets containing the two elements.
   *
   * @param x an element
   * @param y an element
   */
  void union(int x, int y) {
    int rx = find(x);
    int ry = find(y);
    if (rx != ry) {
      parent[ry] = rx;
    }
  }
}
