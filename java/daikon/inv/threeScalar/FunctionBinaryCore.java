package daikon.inv.threeScalar;

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

public class FunctionBinaryCore {

  public Method function;
  // see "Variable order"
  public int var_order;

  // Not currently being maintained
  // int values_seen = 0;

  Invariant wrapper;

  public FunctionBinaryCore(Invariant wrapper_, Method function_, int var_order_) {
    wrapper = wrapper_;
    function = function_;
    var_order = var_order_;
  }

  public void add_modified(int x_int, int y_int, int z_int, int count) {

    Integer x = new Integer(x_int);
    Integer y = new Integer(y_int);
    Integer z = new Integer(z_int);

    Integer result;
    Integer arg1;
    Integer arg2;

    if (var_order == order_xyz) {
      result = x; arg1 = y; arg2 = z;
    } else if (var_order == order_yxz) {
      result = y; arg1 = x; arg2 = z;
    } else if (var_order == order_zxy) {
      result = z; arg1 = x; arg2 = y;
    } else if (var_order == order_xzy) {
      result = x; arg1 = z; arg2 = y;
    } else if (var_order == order_yzx) {
      result = y; arg1 = z; arg2 = x;
    } else if (var_order == order_zyx) {
      result = z; arg1 = y; arg2 = x;
    } else {
      throw new Error("Bad var_order: " + var_order);
    }

    try {
	if (! result.equals(function.invoke(null, new Object[] { arg1, arg2 }))) {
          // System.out.println("FunctionBinaryCore failed: "
          //                    + result + " != " + function + "(" + arg1 + ", " + arg2 + ")"
          //                    + " ; " + var_order_string[var_order]);
	  wrapper.destroy();
          return;
        }
    } catch (Exception e) {
      wrapper.destroy();
      return;
    }
  }


  public double computeProbability() {
    if (wrapper.no_invariant)
      return Invariant.PROBABILITY_NEVER;
    // Not currently being maintained
    // if (values_seen < 5)
    //   return Invariant.PROBABILITY_UNKNOWN;
    // This isn't right, is it?
    // The actual value probably depends on the function.
    return 0;
  }


  /// Variable order

  // These constants indicate which are the arguments.
  // For instance, "order_xyz" indicates the relationship is x=f(y,z).
  static final int order_xyz = 0; // x = f(y,z)
  static final int order_yxz = 1; // y = f(x,z)
  static final int order_zxy = 2; // z = f(x,y)
  static final int order_xzy = 3; // x = f(z,y)
  static final int order_yzx = 4; // y = f(z,x)
  static final int order_zyx = 5; // z = f(y,x)
  static final int order_symmetric_start = order_xyz;
  static final int order_symmetric_max = order_zxy;
  static final int order_nonsymmetric_start = order_xyz;
  static final int order_nonsymmetric_max = order_zyx;

  static final int[][] var_indices;
  static {
    var_indices = new int[order_nonsymmetric_max+1][];
    var_indices[order_xyz] = new int[] { 0, 1, 2 };
    var_indices[order_yxz] = new int[] { 1, 0, 2 };
    var_indices[order_zxy] = new int[] { 2, 0, 1 };
    var_indices[order_xzy] = new int[] { 0, 2, 1 };
    var_indices[order_yzx] = new int[] { 1, 2, 0 };
    var_indices[order_zyx] = new int[] { 2, 1, 0 };
  }

  static final String[] var_order_string = { "x=f(y,z)",
                                             "y=f(x,z)",
                                             "z=f(x,y)",
                                             "x=f(z,y)",
                                             "y=f(z,x)",
                                             "z=f(y,x)" };

}
