package utilMDE;

import java.util.*;
import java.io.*;

/* Graph utility methods.  This class does not model a graph:  all methods
 * are static. */
public class GraphMDE {

  // Algorithms for computing dominators:
  //
  // Wikipedia:
  //  // dominator of the start node is the start itself
  //  Dom(n_0) = {n_0}
  //  // for all other nodes, set all nodes as the dominators
  //  for each n in N - {n_0}
  //      Dom(n) = N;
  //  // iteratively eliminate nodes that are not dominators
  //  while changes in any Dom(n)
  //      for each n in N - {n_0}:
  //          Dom(n) = {n} union with intersection over all p in pred(n) of Dom(p)
  //
  // Cooper/Harvey/Kennedy:
  //  for all nodes, n
  //    DOM[n] := {1 . . .N}
  //  Changed := true
  //  while (Changed)
  //    Changed := false
  //    for all nodes, n, in reverse postorder
  //      new_set := (Intersect_{p:=preds(n)} DOM[p]) union {n}
  //      if (new_set != DOM[n])
  //        DOM[n] := new_set
  //        Changed := true

  // The two algorithms are essentially the same; this implementation uses
  // follows the Wikipedia one.

  /**
   * Computes, for each node in the graph, its set of (pre-)dominators.
   * Supply a successor graph if you want post-dominators.
   **/
  public static
  <T> Map<T,List<T>> dominators(Map<T,List<T>> preds) {

    List<T> nodes = new ArrayList<T>(preds.keySet());

    // Compute roots & non-roots, for convenience
    List<T> roots = new ArrayList<T>();
    List<T> non_roots = new ArrayList<T>();

    Map<T,List<T>> dom = new HashMap<T,List<T>>();

    // Initialize result:  for roots just the root, otherwise everything
    for (T node : preds.keySet()) {
      if (preds.get(node).isEmpty()) {
        // This is a root
        roots.add(node);
        // Its only dominator is itself.
        dom.put(node, new ArrayList<T>(Collections.singleton(node)));
      } else {
        non_roots.add(node);
        dom.put(node, new ArrayList<T>(nodes));
      }
    }
    assert roots.size() + non_roots.size() == nodes.size();

    boolean changed = true;
    while (changed) {
      changed = false;
      for (T node : non_roots) {
        List<T> new_doms = null;
        assert preds.containsKey(node);
        for (T pred : preds.get(node)) {
          assert dom.containsKey(pred);
          /*@NonNull*/ List<T> dom_of_pred = dom.get(pred);
          if (new_doms == null) {
            // make copy because we may side-effect new_doms
            new_doms = new ArrayList<T>(dom_of_pred);
          } else {
            new_doms.retainAll(dom_of_pred);
          }
        }
        new_doms.add(node);
        assert dom.containsKey(node);
        if (! dom.get(node).equals(new_doms)) {
          dom.put(node, new_doms);
          changed = true;
        }
      }
    }

    for (T node : preds.keySet()) {
      assert dom.containsKey(node);
      assert dom.get(node).contains(node);
    }

    return dom;
  }

  public static
  <T> void print(Map<T,List<T>> graph, PrintStream ps, int indent) {
    String indentString = "";
    for (int i=0; i<indent; i++) {
      indentString += " ";
    }
    for (T node : graph.keySet()) {
      ps.printf("%s%s%n", indentString, node);
      for (T child : graph.get(node)) {
        ps.printf("  %s%s%n", indentString, child);
      }
    }
  }

}
