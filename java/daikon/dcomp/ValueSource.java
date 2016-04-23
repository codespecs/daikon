package daikon.dcomp;

import daikon.chicory.*;
import daikon.util.ArraysMDE;
import daikon.util.SimpleLog;
import daikon.util.Stopwatch;
import daikon.util.WeakIdentityHashMap;
import java.io.PrintWriter;
import java.lang.reflect.*;
import java.util.*;
import java.util.regex.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Class used in dataflow that creates a tree the defines the way that
 * each value is created.
 */
@SuppressWarnings("interning")
public class ValueSource {

  /** Description of the value, includes its source if it is a constant **/
  String descr;

  /** Stack trace of the location where this node was created. **/
  Throwable stack_trace;

  /** Left subtree for binary/unary operations **/
  /*@Nullable*/ ValueSource left;

  /** Right subtree for binary operations **/
  /*@Nullable*/ ValueSource right;

  /** ValueSet used for the null reference value **/
  public static ValueSource null_value_source =
      new ValueSource("null", new Throwable().fillInStackTrace());

  private static String blank_string = "                 ";

  private static final String lineSep = System.getProperty("line.separator");

  // ValueSource (String descr) { this.descr = descr; }
  ValueSource(String descr, Throwable stack_trace) {
    this.descr = descr;
    this.stack_trace = stack_trace;
  }

  ValueSource(String descr, Throwable stack_trace, ValueSource left, ValueSource right) {
    this.descr = descr;
    this.stack_trace = stack_trace;
    this.left = left;
    this.right = right;
  }

  /**
   * Returns a map from each test sequence variable (identified by
   * local-stores) to the values they are compared against.  The
   * compare_to param is presumed to be compared to all of the test
   * sequence variables found.
   */
  public Map<String, Set<String>> get_var_compares(String compare_to) {

    // Look to see if there are any comparisons to null.  Add these as
    // compare_to values if they exist.  Note that null is really the
    // only interesting compare-to value at this point.  An object isn't
    // interesting and at this point, we aren't trying to indicate direct
    // test-sequence comparisons (we presume any variables mentioned might
    // be compared)
    Set<String> other_compares = new LinkedHashSet<String>();
    for (ValueSource vs : get_node_list()) {
      if (vs.descr.equals("equals")) {
        if ((vs.left == null_value_source) || (vs.right == null_value_source))
          other_compares.add("null");
      }
    }

    Map<String, Set<String>> var_compares = new LinkedHashMap<String, Set<String>>();
    for (String var : get_vars()) {
      Set<String> compare_to_set = var_compares.get(var);
      if (compare_to_set == null) {
        compare_to_set = new LinkedHashSet<String>();
        var_compares.put(var, compare_to_set);
      }
      compare_to_set.add(compare_to);
      compare_to_set.addAll(other_compares);
    }
    return var_compares;
  }

  /**
   * Returns a set of all of the test sequence variables in the tree
   * The source name of the variable is placed in the set.  This is
   * obtained from the local variable table information for the test
   * sequence
   */
  public Set<String> get_vars() {

    Set<String> varnames = new LinkedHashSet<String>();
    for (ValueSource vs : get_node_list()) {
      // System.out.printf ("get_vars: processing node %s%n", vs.descr);
      if (vs.descr.startsWith("local-store")) {
        int local_index = Integer.decode(vs.descr.split(" ")[1]);
        String local_name = DFInstrument.test_seq_locals[local_index];
        varnames.add(local_name);
      }
    }
    return varnames;
  }

  /**
   * Returns a list of all of the nodes in the tree
   */
  public List<ValueSource> get_node_list() {
    List<ValueSource> vs_list = new ArrayList<ValueSource>();
    add_node_list(vs_list);
    return vs_list;
  }

  /**
   * Add all of the nodes in this tree to vs_list
   */
  private void add_node_list(List<ValueSource> vs_list) {
    vs_list.add(this);
    if (left != null) left.add_node_list(vs_list);
    if (right != null) right.add_node_list(vs_list);
  }

  public Throwable get_stack_trace() {
    return stack_trace;
  }
  /*@SideEffectFree*/ public String toString() {
    String left_descr = "-";
    if (left != null) left_descr = left.toString();
    String right_descr = "-";
    if (right != null) right_descr = right.toString();
    return String.format("(%s %s/%s)", descr, left_descr, right_descr);
  }

  public String tree_dump() {
    StringBuilder out = new StringBuilder();
    return tree_dump(out, 0).toString();
  }

  /** make sure that the blank string is long enough for the specified size **/
  private void ensure_blank_len(int size) {
    if (size > blank_string.length()) {
      while (blank_string.length() < size) blank_string += " ";
    }
  }

  public StringBuilder tree_dump(StringBuilder out, int indent) {

    // Make sure the blank string is long enough for the indent.
    ensure_blank_len(indent + 2);

    out.append(blank_string, 0, indent);
    out.append('-');
    out.append(descr);
    if (stack_trace != null) {
      for (StackTraceElement ste : stack_trace.getStackTrace()) {
        if (ste.getClassName().startsWith("daikon.dcomp.DCRuntime")) continue;
        out.append(lineSep);
        out.append(blank_string, 0, indent + 2);
        out.append(ste);
      }
    }
    if (left != null) {
      left.tree_dump(out, indent + 2);
      out.append(lineSep);
    }

    if (right != null) {
      right.tree_dump(out, indent + 2);
      out.append(lineSep);
    }

    return out;
  }
}
