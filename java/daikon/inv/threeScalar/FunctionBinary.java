package daikon.inv.threeScalar;

import daikon.*;
import daikon.inv.*;
import java.lang.reflect.*;


class FunctionBinary extends ThreeScalar {

  static final boolean debugFunctionBinary = false;

  FunctionBinaryCore core;

  protected FunctionBinary(PptSlice ppt_, Method function_, int var_order_) {
    super(ppt_);
    core = new FunctionBinaryCore(this, function_, var_order_);
  }

  public static FunctionBinary instantiate(PptSlice ppt, Method function, int var_order) {
    {
      int[] indices = FunctionBinaryCore.var_indices[var_order];
      VarInfo argresult = ppt.var_infos[indices[0]];
      VarInfo arg1 = ppt.var_infos[indices[1]];
      VarInfo arg2 = ppt.var_infos[indices[2]];
      if (debugFunctionBinary)
        System.out.println("FunctionBinary.instantiate(" + ppt.name + ", " + function.getName() + ", " + argresult.name + "=" + "f(" + arg1.name + "," + arg2.name + ")" + " )");
    }

    // Skip if the arguments are constant (but not if the result
    // is constant, as we might get something like y=abs(x)).
    int[] indices = FunctionBinaryCore.var_indices[var_order];
    VarInfo arg1 = ppt.var_infos[indices[1]];
    VarInfo arg2 = ppt.var_infos[indices[2]];
    if (arg1.isConstant() && arg2.isConstant()) {
      if (debugFunctionBinary)
        System.out.println("FunctionBinary.instantiate: both args are constant");
      return null;
    }

    return new FunctionBinary(ppt, function, var_order);
  }

  public String repr() {
    Method function = core.function;
    int var_order = core.var_order;

    double probability = getProbability();
    return "FunctionBinary" + varNames() + ": "
      + "function=" + function
      + ",var_order=" + var_order
      + "; probability = " + probability;
  }

  public String format() {
    Method function = core.function;
    int var_order = core.var_order;
    int[] indices = FunctionBinaryCore.var_indices[var_order];
    VarInfo argresult = ppt.var_infos[indices[0]];
    VarInfo arg1 = ppt.var_infos[indices[1]];
    VarInfo arg2 = ppt.var_infos[indices[2]];

    if (justified()) {
      return argresult.name + " = "
        + function.getName() + "(" + arg1.name + ", " + arg2.name + ")";
    } else {
      // System.out.println("FunctionBinary not justified: "
      //                    + argresult.name + " = "
      //                    + function.getName() + "(" + arg1.name + ", " + arg2.name + ")");
      return null;
    }
  }


  public void add_modified(int x_int, int y_int, int z_int, int count) {
    core.add_modified(x_int, y_int, z_int, count);
  }


  protected double computeProbability() {
    return core.computeProbability();
  }

  // For testing only; to be commented out
  public void destroy() {
    Method function = core.function;
    int var_order = core.var_order;

    int[] indices = FunctionBinaryCore.var_indices[var_order];
    VarInfo argresult = ppt.var_infos[indices[0]];
    VarInfo arg1 = ppt.var_infos[indices[1]];
    VarInfo arg2 = ppt.var_infos[indices[2]];

    if (debugFunctionBinary)
      System.out.println("FunctionBinary.destroy: "
                         + argresult.name + " = "
                         + function.getName() + "(" + arg1.name + ", " + arg2.name + ")");
    super.destroy();
  }

}
