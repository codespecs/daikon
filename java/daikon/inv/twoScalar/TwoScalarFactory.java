package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public class TwoScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt) {
    // Not really the right place for this test
    if (!((ppt.var_infos[0].type.dimensions() == 0)
	  && (ppt.var_infos[1].type.dimensions() == 0)))
      return;

    IntComparison.instantiate(ppt);
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

  private TwoScalarFactory() {
  }

}
