package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.Invariant;
import java.lang.reflect.*;


class FunctionUnary extends TwoScalar {

  FunctionUnaryCore core;

  protected FunctionUnary(PptSlice ppt, String methodname, Method function, boolean inverse) {
    super(ppt);
    core = new FunctionUnaryCore(this, methodname, function, inverse);
  }

  public static FunctionUnary instantiate(PptSlice ppt, String methodname, Method function, boolean inverse) {
    return new FunctionUnary(ppt, methodname, function, inverse);
  }

  public String repr() {
    double probability = getProbability();
    return "FunctionUnary" + varNames() + ": "
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


  public void add_modified(long x_int, long y_int, int count) {
    core.add_modified(x_int, y_int, count);
  }


  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((FunctionUnary) other).core);
  }
}
