package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;
import java.io.*;
import java.util.Arrays;
import java.lang.reflect.*;
import utilMDE.*;

// See FunctionUnaryCore for discussion of tradeoffs between constructing
// from java.lang.reflect.Method objects vs. Invokable objects.

public final class FunctionBinaryCore implements java.io.Serializable {

  transient public Method function;
  public final String methodname;
  // see "Variable order"
  public int var_order;

  // Not currently being maintained
  // int values_seen = 0;

  Invariant wrapper;

  public FunctionBinaryCore(Invariant wrapper, String methodname, Method function, int var_order) {
    this.wrapper = wrapper;
    this.methodname = methodname;
    this.function = function;
    this.var_order = var_order;
  }

  public FunctionBinaryCore(Invariant wrapper, String methodname, int var_order) throws ClassNotFoundException, NoSuchMethodException {
    this(wrapper, methodname, UtilMDE.methodForName(methodname), var_order);
  }


  /**
   * Reorganize our already-seen state as if the variables had shifted
   * order underneath us (rearrangement given by the permutation).
   **/
  public void permute(int[] permutation) {
    Assert.assert(permutation.length == 3);
    Assert.assert(ArraysMDE.is_permutation(permutation));
    int[] new_order = new int[3];
    new_order[permutation[0]] = var_indices[var_order][0];
    new_order[permutation[1]] = var_indices[var_order][1];
    new_order[permutation[2]] = var_indices[var_order][2];
    for (int i=0; i < var_indices.length; i++) {
      if (Arrays.equals(new_order, var_indices[i])) {
	var_order = i;
	return;
      }
    }
    Assert.assert(false, "Could not find new ordering");
  }

  private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException, NoSuchMethodException {
    in.defaultReadObject();
    this.set_function();
  }

  private void set_function() throws ClassNotFoundException, NoSuchMethodException {
    Assert.assert(function == null);
    function = UtilMDE.methodForName(methodname);
  }

  public void add_modified(long x_int, long y_int, long z_int, int count) {

    Long x = new Long(x_int);
    Long y = new Long(y_int);
    Long z = new Long(z_int);

    Long result;
    Long arg1;
    Long arg2;

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
    return Invariant.prob_is_ge(wrapper.ppt.num_values(), 5);
  }


  /// Variable order

  // These constants indicate which are the arguments.
  // For instance, "order_xyz" indicates the relationship is x=f(y,z).
  final static int order_xyz = 0; // x = f(y,z)
  final static int order_yxz = 1; // y = f(x,z)
  final static int order_zxy = 2; // z = f(x,y)
  final static int order_xzy = 3; // x = f(z,y)
  final static int order_yzx = 4; // y = f(z,x)
  final static int order_zyx = 5; // z = f(y,x)
  final static int order_symmetric_start = order_xyz;
  final static int order_symmetric_max = order_zxy;
  final static int order_nonsymmetric_start = order_xyz;
  final static int order_nonsymmetric_max = order_zyx;

  final static int[][] var_indices;
  static {
    var_indices = new int[order_nonsymmetric_max+1][];
    var_indices[order_xyz] = new int[] { 0, 1, 2 };
    var_indices[order_yxz] = new int[] { 1, 0, 2 };
    var_indices[order_zxy] = new int[] { 2, 0, 1 };
    var_indices[order_xzy] = new int[] { 0, 2, 1 };
    var_indices[order_yzx] = new int[] { 1, 2, 0 };
    var_indices[order_zyx] = new int[] { 2, 1, 0 };
  }

  final static String[] var_order_string = { "x=f(y,z)",
                                             "y=f(x,z)",
                                             "z=f(x,y)",
                                             "x=f(z,y)",
                                             "y=f(z,x)",
                                             "z=f(y,x)" };

  public String repr() {
    return "FunctionBinaryCore" + wrapper.varNames() + ": "
      + "function=" + function
      + ",var_order=" + var_order;
  }

  // Perhaps this should take arguments rather than looking into the wrapper.
  public String format() {
    PptSlice ppt = wrapper.ppt;
    VarInfo argresult = ppt.var_infos[var_indices[var_order][0]];
    VarInfo arg1 = ppt.var_infos[var_indices[var_order][1]];
    VarInfo arg2 = ppt.var_infos[var_indices[var_order][2]];

    return argresult.name + " == "
      + methodname + "(" + arg1.name + ", " + arg2.name + ")";
  }

  /* IOA */
  public String format_ioa(String cname) {
    PptSlice ppt = wrapper.ppt;
    String result = ppt.var_infos[var_indices[var_order][0]].name.ioa_name(cname);
    String arg1 = ppt.var_infos[var_indices[var_order][1]].name.ioa_name(cname);
    String arg2 = ppt.var_infos[var_indices[var_order][2]].name.ioa_name(cname);

    return result + " = " + methodname + "(" + arg1 + ", " + arg2 + ") ***";
  }

  public String format_esc() {
    return "warning: method " + this.getClass() + ".format_esc() needs to be implemented: " + format();
  }

  public boolean isSameFormula(FunctionBinaryCore other)
  {
    return methodname.equals(other.methodname);
  }

}
