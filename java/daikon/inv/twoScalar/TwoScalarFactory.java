package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public class TwoScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    Assert.assert((! var1.type.isArray())
                  && (! var2.type.isArray()));

    if (pass == 1) {
      IntComparison.instantiate(ppt);
    } else if (pass == 2) {
      for (int i=0; i<2; i++) {
        boolean invert = (i==1);
        VarInfo arg = (invert ? var1 : var2);
        // Skip if the argument is a constant (but not if the result
        // is constant, as we might get something like y=abs(x)).
        if (! arg.isConstant()) {
          Function.instantiate(ppt, Functions.Math_abs, invert);
          Function.instantiate(ppt, Functions.MathMDE_negate, invert);
          Function.instantiate(ppt, Functions.MathMDE_bitwiseComplement, invert);
        }
      }
      if ((! var1.isConstant()) && (! var2.isConstant())) {
        Linear.instantiate(ppt);
        // new NonAliased(ppt);
        NonEqual.instantiate(ppt);
      }
    }
  }

  private TwoScalarFactory() {
  }

}
