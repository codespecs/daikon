package daikon.inv.threeScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public class ThreeScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo var3 = ppt.var_infos[1];

    Assert.assert((! var1.type.isArray())
                  && (! var2.type.isArray())
                  && (! var3.type.isArray()));

    if (pass == 1) {
      // nothing to do
    } else if (pass == 2) {
      for (int var_order = FunctionBinaryCore.order_symmetric_start;
           var_order <= FunctionBinaryCore.order_symmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binarySymmetricFunctions.length; j++) {
          FunctionBinary.instantiate(ppt, Functions.binarySymmetricFunctions[j], var_order);
        }
      }
      for (int var_order = FunctionBinaryCore.order_nonsymmetric_start;
           var_order <= FunctionBinaryCore.order_nonsymmetric_max;
           var_order++) {
        for (int j=0; j<Functions.binaryNonSymmetricFunctions.length; j++) {
          FunctionBinary.instantiate(ppt, Functions.binaryNonSymmetricFunctions[j], var_order);
        }
      }
      if ((! var1.isConstant()) && (! var2.isConstant()) && (! var3.isConstant())) {
        // LinearTernary.instantiate(ppt);
      }
    }
  }

  private ThreeScalarFactory() {
  }

}
