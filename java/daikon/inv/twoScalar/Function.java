package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;
import java.lang.reflect.*;


class Function extends TwoScalar {

  FunctionCore core;

  protected Function(PptSlice ppt_, Method function_, boolean inverse_) {
    super(ppt_);
    core = new FunctionCore(this, function_, inverse_);
  }

  public static Function instantiate(PptSlice ppt, Method function, boolean inverse) {
    return new Function(ppt, function, inverse);
  }

  public String repr() {
    Method function = core.function;
    boolean inverse = core.inverse;

    double probability = getProbability();
    return "Function" + varNames() + ": "
      + "function=" + function
      + ",inverse=" + inverse
      + "; probability = " + probability;
  }

  public String format() {
    Method function = core.function;
    boolean inverse = core.inverse;

    if (justified()) {
      String argname = inverse ? var2().name : var1().name;
      String resultname = inverse ? var1().name : var2().name;
      return resultname + " = " + function + "(" + argname + ")";
    } else {
      return null;
    }
  }


  public void add_modified(int x_int, int y_int, int count) {
    core.add_modified(x_int, y_int, count);
  }


  protected double computeProbability() {
    return core.computeProbability();
  }

}
