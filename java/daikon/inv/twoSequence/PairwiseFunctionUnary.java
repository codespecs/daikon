package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.twoScalar.*;
import java.lang.reflect.*;

// Two basic options here:
//     * take java.lang.reflect.Method objects
//        + that fits in well with pre-existing methods.  It also does wrapping
//          and unwrapping as appropriate
//        + I must provide an object or arrange that the methods are all static,
//          so I might end up with a new object of my own type after all.
//     * take Invokable objects, where I define a new interface with invoke() method
//        + I might not want wrapping, unwrapping, and args wrapped in Object[]:
//          more efficient not to do so
//        + might be more efficient to define my own interface with specialized
//          types, not Object
//        - must convert existing functions to this format.

class PairwiseFunctionUnary extends TwoSequence {

  FunctionUnaryCore core;

  protected PairwiseFunctionUnary(PptSlice ppt_, Method function_, boolean inverse_) {
    super(ppt_);
    core = new FunctionUnaryCore(this, function_, inverse_);
  }

  public static PairwiseFunctionUnary instantiate(PptSlice ppt, Method function, boolean inverse) {
    return new PairwiseFunctionUnary(ppt, function, inverse);
  }

  public String repr() {
    Method function = core.function;
    boolean inverse = core.inverse;

    double probability = getProbability();
    return "PairwiseFunctionUnary" + varNames() + ": "
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


  public void add_modified(int[] x_arr, int[] y_arr, int count) {
    int len = Math.min(x_arr.length, y_arr.length);

    for (int i=0; i<len; i++) {
      int x = x_arr[i];
      int y = y_arr[i];

      core.add_modified(x, y, count);
      if (no_invariant)
        return;
    }
  }


  protected double computeProbability() {
    return core.computeProbability();
  }

}
