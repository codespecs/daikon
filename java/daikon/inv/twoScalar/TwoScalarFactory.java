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

    new IntComparison(ppt);
    for (int i=0; i<2; i++) {
      boolean b = (i==1);
      new Function(ppt, Functions.Math_abs, b);
      new Function(ppt, Functions.MathMDE_negate, b);
      new Function(ppt, Functions.MathMDE_bitwiseComplement, b);
    }
    new Linear(ppt);
    // new NonAliased(ppt);
    new NonEqual(ppt);
  }

  private TwoScalarFactory() {
  }

}
