package daikon.inv.twoScalar;

import daikon.inv.*;
import java.io.*;
import java.lang.reflect.*;
import utilMDE.*;

// Two basic options here:
//     * take java.lang.reflect.Method objects
//        + that fits in well with pre-existing methods.  It also does wrapping
//          and unwrapping as appropriate
//        - I must provide an object or arrange that the methods are all static,
//          so I might end up with a new object of my own type after all.
//     * take Invokable objects, where I define a new interface with invoke() method
//        + I might not want wrapping, unwrapping, and args wrapped in Object[]:
//          more efficient not to do so
//        + might be more efficient to define my own interface with specialized
//          types, not Object
//        - must convert existing functions to this format.

public final class FunctionUnaryCore implements java.io.Serializable {

  transient public Method function;  // transient:  Method not serializable
  public final String methodname;   // provided to permit serialization


  // false if we're looking for y=fn(x), true if we're looking for x=fn(y)
  public boolean inverse;

  // Not currently being maintained
  // int values_seen = 0;

  Invariant wrapper;

  public FunctionUnaryCore(Invariant wrapper_, String methodname_, Method function_, boolean inverse_) {
    wrapper = wrapper_;
    methodname = methodname_;
    function = function_;
    inverse = inverse_;
  }

  public FunctionUnaryCore(Invariant wrapper_, String methodname_, boolean inverse_) throws ClassNotFoundException, NoSuchMethodException {
    this(wrapper_, methodname_, UtilMDE.methodForName(methodname_), inverse_);
  }

  private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException, NoSuchMethodException {
    in.defaultReadObject();
    this.set_function();
  }

  private void set_function() throws ClassNotFoundException, NoSuchMethodException {
    Assert.assert(function == null);
    function = UtilMDE.methodForName(methodname);
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
    if (wrapper.ppt.num_values() < 5)
      return Invariant.PROBABILITY_UNKNOWN;
    // The actual value probably depends on the function.
    return Invariant.PROBABILITY_JUSTIFIED;
  }


  public String format(String vname1, String vname2) {
    String argname = inverse ? vname2 : vname1;
    String resultname = inverse ? vname1 : vname2;
    return resultname + " = " + function + "(" + argname + ")";
  }

  public String repr() {
    return "FunctionUnaryCore" + wrapper.varNames() + ": "
      + "function=" + function
      + ",inverse=" + inverse;
  }

}
