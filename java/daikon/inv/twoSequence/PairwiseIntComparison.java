package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.twoScalar.*;

// Requires that the lengths are the same.  Should it?  (Hard to tell;
// could argue either way, on completeness vs. efficiency grounds.)

// Also see NonEqual
class PairwiseIntComparison extends TwoSequence {

  final static boolean debugPairwiseIntComparison = false;

  IntComparisonCore core;

  protected PairwiseIntComparison(PptSlice ppt) {
    super(ppt);
    core = new IntComparisonCore(this);
  }

  protected PairwiseIntComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    core = new IntComparisonCore(this, only_eq);
  }

  public static PairwiseIntComparison instantiate(PptSlice ppt) {
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    boolean only_eq = false;
    if (! (var1.type.elementType().isIntegral() && var2.type.elementType().isIntegral())) {
      only_eq = true;
    }

    return new PairwiseIntComparison(ppt, only_eq);
  }

  public String repr() {
    double probability = getProbability();
    return "PairwiseIntComparison(" + var1().name + "," + var2().name + "): "
      + "probability = " + probability
      + "; " + core.repr();
  }

  public String format() {
    if (! justified()) {
      return null;
    }

    String comparator = core.format_comparator();
    if (comparator == null) {
      return null;
    }
    return var1().name + " " + comparator + " " + var2().name
      + " (elementwise)";
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    if (a1.length != a2.length) {
      destroy();
      return;
    }
    int len = a1.length;
    // int len = Math.min(a1.length, a2.length);

    for (int i=0; i<len; i++) {
      int v1 = a1[i];
      int v2 = a2[i];
      core.add_modified(v1, v2, count);
      if (no_invariant)
        return;
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

}
