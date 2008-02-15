package daikon.asm;

import java.util.*;

class DSForest <T> {

  IdentityHashMap<T, DSForestNode> elements =
    new IdentityHashMap<T, DSForestNode>();

  public static int idCounter = 0;
  
  // We don't want to iterate over IdentityHashMap because
  // its iterative behavior is non-deterministic. So we
  // keep the nodes as a separate list.
  List<DSForestNode> nodes = new ArrayList<DSForestNode>();

  private class DSForestNode {
    public T element;
    public DSForestNode parent;
    public int rank;
    public int id;
    
    public DSForestNode(T element) {
      this.element = element;
      this.parent = this;
      this.rank = 0;
      this.id = idCounter;
      idCounter++;
    }
  }

  // If e is not already in the forest,
  //   makes a new singleton set containing e
  // Else
  //   throws an IllegalArgumentException.
  public void makeSet(T e) {
    if (elements.containsKey(e))
      throw new IllegalArgumentException("Element already in forest.");
    DSForestNode n = new DSForestNode(e);
    elements.put(e, n);
    nodes.add(n);
  }

  private DSForestNode find(DSForestNode n) {
    if (n.parent == n) {
      return n;
    } else {
      n.parent = find(n.parent);
      return n.parent;
    }
  }

  // x and y must be elements of the forest.
  // If e1 == e2, or e1 and e2 are in the same set, has no effect.
  // Otherwise, unions the sets containing e1 and e2.
  public void union(T x, T y) {
    DSForestNode xNode = elements.get(x);
    if (xNode == null) throw new IllegalArgumentException();
    DSForestNode yNode = elements.get(y);
    if (yNode == null) throw new IllegalArgumentException();
    DSForestNode xRoot = find(xNode);
    DSForestNode yRoot = find(yNode);
    assert x == y ? xRoot == yRoot : true;
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
  public List<List<T>> asLists() {

    Map<Integer, List<T>> sets =
      new LinkedHashMap<Integer, List<T>>();

    for (DSForestNode n : nodes) {
      int rootId = find(n).id;
      List<T> set = sets.get(rootId);
      if (set == null) {
        set = new ArrayList<T>();
        sets.put(rootId, set);
      }
      set.add(n.element);
    }

    List<List<T>> retval = new ArrayList<List<T>>();
    for (List<T> l : sets.values()) {
      retval.add(l);
    }
    return retval;
  }
}
