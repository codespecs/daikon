package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.twoScalar.*;

class PairwiseLinear extends TwoSequence {

  final static boolean debug_PairwiseLinear = false;

  LinearCore core;

  public PairwiseLinear(PptSlice ppt_) {
    super(ppt_);
    core = new LinearCore();
  }

  // Need to add these two methods for all subclasses of Invariant
  public String name() {
    return "PairwiseLinear" + varNames();
  }
  public String long_name() {
    return name() + "@" + ppt.name;
  }

  public String repr() {
    int a = core.a;
    int b = core.b;
    double probability = getProbability();
    return "PairwiseLinear" + varNames() + ": "
      + "no_invariant=" + core.no_invariant
      + ",a=" + core.a
      + ",b=" + core.b
      + "; probability = " + probability;
  }

  public String format() {
    boolean no_invariant = core.no_invariant;
    int a = core.a;
    int b = core.b;

    if ((!no_invariant) && justified()) {
      String x = var1().name;
      String y = var2().name;
      String b_rep = (b<0) ? (" - " + -b) : (b>0) ? (" + " + b) : "";
      String a_rep = (a==1) ? "" : ("" + a + " * ");
      return y + " = " + a_rep + x + b_rep;
    } else {
      return null;
    }
  }

  public void add_modified(int[] x_arr, int[] y_arr, int count) {
    boolean no_invariant = core.no_invariant;
    int a = core.a;
    int b = core.b;

    if (no_invariant) {
      return;
    }

    int len = Math.min(x_arr.length, y_arr.length);

    for (int i=0; i<len; i++) {
      int x = x_arr[i];
      int y = y_arr[i];

      core.add_modified(x, y, count);
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

}
