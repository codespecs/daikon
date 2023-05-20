package daikon.inv;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;

/**
 * Container class for holding all info needed to describe an Invariant. If any field is null, that
 * field is a wildcard, so one instance of InvariantInfo may describe multiple Invariants in that
 * way.
 */
public class InvariantInfo {

  private String ppt;

  // vars is maintained as "var1,var2,.." sorted in ascending lexicographical order
  /** If null, treated as a wildcard. */
  private final @Nullable String vars;

  /** If null, treated as a wildcard. */
  private @Nullable String className;

  //   private InvariantInfo() {
  //     // Make the default constructor private, this should never be called
  //   }

  /* // It's ok if vars isn't given in sorted order, we'll sort it here */
  public InvariantInfo(String ppt, @Nullable String vars, @Nullable String className) {
    this.ppt = ppt;
    this.className = className;
    /* if (vars != null) {
    // Sort the vars into ascending order
    StringTokenizer st = new StringTokenizer(vars, ",");
    ArrayList temp = new ArrayList();
    while (st.hasMoreTokens()) {
      temp.add(st.nextToken());
    }
    Collections.sort(temp);
    String vars_result = "";
    for (int i = 0; i < temp.size(); i++) {
      vars_result += temp.get(i) + ",";
    }
    this.vars = vars_result;
    }*/
    this.vars = vars;
  }

  public String ppt() {
    return this.ppt;
  }

  @Pure
  public @Nullable String className() {
    return this.className;
  }

  public @Nullable String vars() {
    return this.vars;
  }

  /**
   * Returns a list of Strings of all permutations of the vars, or null if vars == null.
   *
   * <p>e.g., if vars is "var1,var2,var3", this method will return ["var1,var2,var3",
   * "var1,var3,var2", "var2,var1,var3"... etc.]
   *
   * @return a list of Strings of all permutations of the vars, or null if vars == null
   */
  public @Nullable List<String> var_permutations() {
    if (vars == null) {
      return null;
    }

    // We know there can be at most 3 vars so it's not worth writing
    // a complicated routine that generates all permutations
    StringTokenizer st = new StringTokenizer(vars, ",");
    assert st.countTokens() <= 3;
    ArrayList<String> result = new ArrayList<>(3);
    if (st.countTokens() == 1) {
      result.add(vars);
      return result;
    } else if (st.countTokens() == 2) {
      result.add(vars);
      String var1 = st.nextToken();
      String var2 = st.nextToken();
      result.add(var2 + "," + var1);
      return result;
    } else {
      // st.countTokens() == 3
      String var1 = st.nextToken();
      String var2 = st.nextToken();
      String var3 = st.nextToken();
      result.add(var1 + "," + var2 + "," + var3);
      result.add(var1 + "," + var3 + "," + var2);
      result.add(var2 + "," + var1 + "," + var3);
      result.add(var2 + "," + var3 + "," + var1);
      result.add(var3 + "," + var1 + "," + var2);
      result.add(var3 + "," + var2 + "," + var1);
      return result;
    }
  }
}
