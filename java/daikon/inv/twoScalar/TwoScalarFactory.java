package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public class TwoScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    Assert.assert((!ppt.var_infos[0].type.isArray())
                  && (!ppt.var_infos[1].type.isArray()));

    if (pass == 1) {
      IntComparison.instantiate(ppt);
    } else if (pass == 2) {
      for (int i=0; i<2; i++) {
        boolean b = (i==1);
        Function.instantiate(ppt, Functions.Math_abs, b);
        Function.instantiate(ppt, Functions.MathMDE_negate, b);
        Function.instantiate(ppt, Functions.MathMDE_bitwiseComplement, b);
      }
      Linear.instantiate(ppt);
      // new NonAliased(ppt);
      NonEqual.instantiate(ppt);
    }
  }

  private TwoScalarFactory() {
  }

}
