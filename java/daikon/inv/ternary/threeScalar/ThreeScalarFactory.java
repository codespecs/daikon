package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;


import org.apache.log4j.Category;


public final class ThreeScalarFactory {

  /**
   * Debug tracer
   **/
  final static Category debug = Category.getInstance ("daikon.inv.ternary.threeScalar.ThreeScalarFactory");


  public final static int max_instantiate
    =  ((Functions.binarySymmetricFunctions.length
         * FunctionBinaryCore.order_symmetric_max)
        + (Functions.binaryNonSymmetricFunctions.length
           * FunctionBinaryCore.order_nonsymmetric_max));

  // Add the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo var3 = ppt.var_infos[2];

    Assert.assert(var1.rep_type.isIntegral()
                  && var2.rep_type.isIntegral()
                  && var3.rep_type.isIntegral());

    if (debug.isDebugEnabled()) {
      debug.debug ("Instantiating for " + ppt.name);
      debug.debug ("Vars: " + var1.name + " " + var2.name + " " + var3.name);
    }

    if (! var1.compatible(var2)) {
      debug.debug ("Not comparable 1 to 2.  Returning");
      return null;
    }
    if (! var2.compatible(var3)) {
      debug.debug ("Not comparable 2 to 3.  Returning");
      return null;
    }
    // Check transitivity of "compatible" relationship.
    Assert.assert(var1.compatible(var3));

    {
      Vector result = new Vector();
      for (int var_order = FunctionBinaryCore.order_symmetric_start;
           var_order <= FunctionBinaryCore.order_symmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binarySymmetricFunctions.length; j++) {
          FunctionBinary fb = FunctionBinary.instantiate(ppt, Functions.binarySymmetricFunctionNames[j], Functions.binarySymmetricFunctions[j], var_order);
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
        for (int j=0; j<Functions.binaryNonSymmetricFunctions.length; j++) {
          FunctionBinary fb = FunctionBinary.instantiate(ppt, Functions.binaryNonSymmetricFunctionNames[j], Functions.binaryNonSymmetricFunctions[j], var_order);
          // no need to increment noninstantiated-invariants counters if
          // null; they were already incremented.
          if (fb != null)
            result.add(fb);
        }
      }
      result.add(LinearTernary.instantiate(ppt));
      if (debug.isDebugEnabled()) {
        debug.debug ("Instantiated invs " + result);
      }
      return result;
    }
  }

  private ThreeScalarFactory() {
  }

}
