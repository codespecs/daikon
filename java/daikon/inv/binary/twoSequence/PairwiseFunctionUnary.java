package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;
import java.lang.reflect.*;


class PairwiseFunctionUnary extends TwoSequence {

  FunctionUnaryCore core;

  protected PairwiseFunctionUnary(PptSlice ppt, String methodname, Method function, boolean inverse) {
    super(ppt);
    core = new FunctionUnaryCore(this, methodname, function, inverse);
  }

  public static PairwiseFunctionUnary instantiate(PptSlice ppt, String methodname, Method function, boolean inverse) {
    return new PairwiseFunctionUnary(ppt, methodname, function, inverse);
  }

  public String repr() {
    double probability = getProbability();
    return "PairwiseFunctionUnary" + varNames() + ": "
      + "probability = " + probability
      + "; " + core.repr();
  }

  public String format() {
    if (justified()) {
      return core.format(var1().name, var2().name);
    } else {
      return null;
    }
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
      if (no_invariant)
        return;
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((PairwiseFunctionUnary) other).core);
  }
  
}
