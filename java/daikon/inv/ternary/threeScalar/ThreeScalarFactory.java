package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;


import java.util.logging.Logger;
import java.util.logging.Level;


public final class ThreeScalarFactory {

  /**
   * Debug tracer
   **/
  final static Logger debug = Logger.getLogger("daikon.inv.ternary.threeScalar.ThreeScalarFactory");


  public final static int max_instantiate
    =  ((Functions.binarySymmetricFunctionNames.length
         * FunctionBinaryCore.order_symmetric_max)
        + (Functions.binaryNonSymmetricFunctionNames.length
           * FunctionBinaryCore.order_nonsymmetric_max));

  // Add the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo var3 = ppt.var_infos[2];

    Assert.assertTrue((var1.rep_type == ProglangType.INT)
                  && (var2.rep_type == ProglangType.INT)
                  && (var3.rep_type == ProglangType.INT));

    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Instantiating for " + ppt.name);
      debug.fine ("Vars: " + var1.name + " " + var2.name + " " + var3.name);
    }

    if (! var1.compatible(var2)) {
      debug.fine ("Not comparable 1 to 2.  Returning");
      return null;
    }
    if (! var2.compatible(var3)) {
      debug.fine ("Not comparable 2 to 3.  Returning");
      return null;
    }
    // Check transitivity of "compatible" relationship.
    Assert.assertTrue(var1.compatible(var3));

    { // previously only if (pass == 2)
      // FIXME for equality
      Vector result = new Vector();
      for (int var_order = FunctionBinaryCore.order_symmetric_start;
           var_order <= FunctionBinaryCore.order_symmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binarySymmetricFunctionNames.length; j++) {
          FunctionBinary fb = FunctionBinary.instantiate(ppt, Functions.binarySymmetricFunctionNames[j], j, var_order);
          // no need to increment noninstantiated-invariants counters if
          // null; they were already incremented.
          if (fb != null) {
            result.add(fb);
          }
        }
      }
      for (int var_order = FunctionBinaryCore.order_nonsymmetric_start;
           var_order <= FunctionBinaryCore.order_nonsymmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binaryNonSymmetricFunctionNames.length; j++) {
          FunctionBinary fb = FunctionBinary.instantiate(ppt, Functions.binaryNonSymmetricFunctionNames[j], j+Functions.binarySymmetricFunctionNames.length, var_order);
          // no need to increment noninstantiated-invariants counters if
          // null; they were already incremented.
          if (fb != null)
            result.add(fb);
        }
      }
      result.add(LinearTernary.instantiate(ppt));
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("Instantiated invs " + result);
      }
      return result;
    }
  }

  private ThreeScalarFactory() {
  }

}
