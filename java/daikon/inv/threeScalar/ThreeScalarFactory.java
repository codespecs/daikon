package daikon.inv.threeScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

public class ThreeScalarFactory {

  public final static int max_instantiate
    =  ((Functions.binarySymmetricFunctions.length
         * FunctionBinaryCore.order_symmetric_max)
        + (Functions.binaryNonSymmetricFunctions.length
           * FunctionBinaryCore.order_nonsymmetric_max));

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo var3 = ppt.var_infos[1];

    Assert.assert((var1.rep_type == ProglangType.INT)
                  && (var2.rep_type == ProglangType.INT)
                  && (var3.rep_type == ProglangType.INT));

    if (Daikon.check_program_types
        && (! (var1.type.comparable(var2.type)
               && var2.type.comparable(var3.type))))
      return null;
    Assert.assert(var1.type.comparable(var3.type));

    if (pass == 1) {
      return null;
      // nothing to do
    } else {
      Assert.assert(pass == 2);
      Vector result = new Vector();
      for (int var_order = FunctionBinaryCore.order_symmetric_start;
           var_order <= FunctionBinaryCore.order_symmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binarySymmetricFunctions.length; j++) {
          FunctionBinary fb = FunctionBinary.instantiate(ppt, Functions.binarySymmetricFunctions[j], var_order);
          if (fb != null)
            result.add(fb);
        }
      }
      for (int var_order = FunctionBinaryCore.order_nonsymmetric_start;
           var_order <= FunctionBinaryCore.order_nonsymmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binaryNonSymmetricFunctions.length; j++) {
          FunctionBinary fb = FunctionBinary.instantiate(ppt, Functions.binaryNonSymmetricFunctions[j], var_order);
          if (fb != null)
            result.add(fb);
        }
      }
      // if (var1.isConstant() || var2.isConstant() || var3.isConstant()) {
      //   Global.subexact_noninstantiated_invariants++;
      // } else {
      //   result.add(LinearTernary.instantiate(ppt));
      // }
      return result;
    }
  }

  private ThreeScalarFactory() {
  }

}
