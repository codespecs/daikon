package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import java.lang.reflect.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public class TwoSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt) {
    // Not really the right place for this test
    if (!(ppt.var_infos[0].rep_type.isArray()
	  && ppt.var_infos[1].rep_type.isArray()))
      return;

    new SeqComparison(ppt);
    // new NonEqual(ppt);
    new SubSequence(ppt);
    new SuperSequence(ppt);
    new Reverse(ppt);

    new PairwiseIntComparison(ppt);
    new PairwiseLinear(ppt);
    for (int i=0; i<2; i++) {
      boolean b = (i==1);
      new PairwiseFunction(ppt, Functions.Math_abs, b);
      new PairwiseFunction(ppt, Functions.MathMDE_negate, b);
      new PairwiseFunction(ppt, Functions.MathMDE_bitwiseComplement, b);
    }

  }

  private TwoSequenceFactory() {
  }

}
