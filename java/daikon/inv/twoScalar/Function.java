package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;
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

class Function extends TwoScalar {

  Method function;
  // false if we're looking for y=fn(x), true if we're looking for x=fn(y)
  boolean inverse;

  boolean no_invariant = false;
  int values_seen = 0;

  public Function(PptSlice ppt_, Method function_, boolean inverse_) {
    super(ppt_);
    function = function_;
    inverse = inverse_;
  }

//   public Function(Ppt ppt_, VarInfo var_info1_, VarInfo var_info2_, Method function_, boolean inverse_) {
//     super(ppt_, var_info1_, var_info2_);
//     function = function_;
//     inverse = inverse_;
//   }


  public String repr() {
    double probability = getProbability();
    return "Function" + varNames() + ": "
      + "function=" + function
      + ",inverse=" + inverse
      + "; probability = " + probability;
  }

  public String format() {
    if (justified()) {
      String argname = inverse ? var2().name : var1().name;
      String resultname = inverse ? var1().name : var2().name;
      return resultname + " = " + function + "(" + argname + ")";
    } else {
      return null;
    }
  }


  public void add_modified(int x_int, int y_int, int count) {
    if (no_invariant) {
      return;
    }

    Integer x = new Integer(x_int);
    Integer y = new Integer(y_int);

    try {
      if (inverse) {
	if (! x.equals(function.invoke(null, new Object[] { y })))
	  no_invariant = true;
      } else {
	if (! y.equals(function.invoke(null, new Object[] { x })))
	  no_invariant = true;
      }
    } catch (Exception e) {
      no_invariant = true;
    }

  }


  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    if (values_seen < 5)
      return Invariant.PROBABILITY_UNKNOWN;
    // This isn't right, is it?
    // The actual value probably depends on the function.
    return 0;
  }


}
