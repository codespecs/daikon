package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;

public class PairwiseLinearBinary extends TwoSequence {

  LinearBinaryCore core;

  protected PairwiseLinearBinary(PptSlice ppt) {
    super(ppt);
    core = new LinearBinaryCore(this);
  }

  public static PairwiseLinearBinary instantiate(PptSlice ppt) {
    return new PairwiseLinearBinary(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "PairwiseLinearBinary" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + ",probability = " + probability
      + "; " + core.repr();
  }

  public String format() {
    return core.format(var1().name, var2().name);
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }

  public void add_modified(long[] x_arr, long[] y_arr, int count) {
    if (x_arr.length != y_arr.length) {
      destroy();
      return;
    }
    int len = x_arr.length;
    // int len = Math.min(x_arr.length, y_arr.length);

    for (int i=0; i<len; i++) {
      long x = x_arr[i];
      long y = y_arr[i];

      core.add_modified(x, y, count);
      if (no_invariant) {
        // destroy() must have already been called
        return;
      }
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((PairwiseLinearBinary) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof PairwiseLinearBinary) {
      return core.isExclusiveFormula(((PairwiseLinearBinary) other).core);
    }
    return false;
  }

}
