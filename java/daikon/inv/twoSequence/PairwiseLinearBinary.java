package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.twoScalar.*;

class PairwiseLinearBinary extends TwoSequence {

  LinearBinaryCore core;

  protected PairwiseLinearBinary(PptSlice ppt_) {
    super(ppt_);
    core = new LinearBinaryCore(this);
  }

  public static PairwiseLinearBinary instantiate(PptSlice ppt) {
    return new PairwiseLinearBinary(ppt);
  }

  // Need to add these two methods for all subclasses of Invariant
  public String name() {
    return "PairwiseLinearBinary" + varNames();
  }
  public String long_name() {
    return name() + "@" + ppt.name;
  }

  public String repr() {
    int a = core.a;
    int b = core.b;
    double probability = getProbability();
    return "PairwiseLinearBinary" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + ",a=" + core.a
      + ",b=" + core.b
      + "; probability = " + probability;
  }

  public String format() {
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
