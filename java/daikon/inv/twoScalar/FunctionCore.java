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

public class FunctionCore {

  public Method function;
  // false if we're looking for y=fn(x), true if we're looking for x=fn(y)
  public boolean inverse;

  int values_seen = 0;

  Invariant wrapper;

  public FunctionCore(Invariant wrapper_, Method function_, boolean inverse_) {
    wrapper = wrapper_;
    function = function_;
    inverse = inverse_;
  }

  public void add_modified(int x_int, int y_int, int count) {

    Integer x = new Integer(x_int);
    Integer y = new Integer(y_int);

    try {
      if (inverse) {
	if (! x.equals(function.invoke(null, new Object[] { y }))) {
	  wrapper.destroy();
          return;
        }
      } else {
	if (! y.equals(function.invoke(null, new Object[] { x }))) {
	  wrapper.destroy();
          return;
        }
      }
    } catch (Exception e) {
      wrapper.destroy();
      return;
    }
  }


  public double computeProbability() {
    if (wrapper.no_invariant)
      return Invariant.PROBABILITY_NEVER;
    if (values_seen < 5)
      return Invariant.PROBABILITY_UNKNOWN;
    // This isn't right, is it?
    // The actual value probably depends on the function.
    return 0;
  }


}
