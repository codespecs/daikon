package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

public final class ThreeScalarFactory {

  public final static int max_instantiate
    =  ((Functions.binarySymmetricFunctions.length
         * FunctionBinaryCore.order_symmetric_max)
        + (Functions.binaryNonSymmetricFunctions.length
           * FunctionBinaryCore.order_nonsymmetric_max));

  // Add the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo var3 = ppt.var_infos[1];

    Assert.assert((var1.rep_type == ProglangType.INT)
                  && (var2.rep_type == ProglangType.INT)
                  && (var3.rep_type == ProglangType.INT));

    // Save ourselves some trouble and never compute threeScalar
    // invariants over pointer types.
    if (!(var1.type.isIntegral() && var2.type.isIntegral() && var3.type.isIntegral())) {
      return null;
    }
	
    if (! var1.compatible(var2))
      return null;
    if (! var2.compatible(var3))
      return null;
    // Check transitivity of "compatible" relationship.
    Assert.assert(var1.compatible(var3));

    if (pass == 1) {
      // nothing to do
      return null;
    } else {
      Assert.assert(pass == 2);
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
      if (var1.isConstant() || var2.isConstant() || var3.isConstant()) {
        Global.subexact_noninstantiated_invariants++;
      } else {
        result.add(LinearTernary.instantiate(ppt));
      }
      return result;
    }
  }

  private ThreeScalarFactory() {
  }

}
