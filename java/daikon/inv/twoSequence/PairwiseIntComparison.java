package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.twoScalar.*;


// Also see NonEqual, NonAliased
class PairwiseIntComparison extends TwoSequence {

  final static boolean debugPairwiseIntComparison = false;

  IntComparisonCore core;

  protected PairwiseIntComparison(PptSlice ppt_) {
    super(ppt_);
    core = new IntComparisonCore(this);
  }

  public static PairwiseIntComparison instantiate(PptSlice ppt) {
    return new PairwiseIntComparison(ppt);
  }

  public String repr() {
    boolean can_be_eq = core.can_be_eq;
    boolean can_be_lt = core.can_be_lt;
    boolean can_be_gt = core.can_be_gt;

    double probability = getProbability();
    return "PairwiseIntComparison(" + var1().name + "," + var2().name + "): "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + "; probability = " + probability;
  }

  public String format() {
    boolean can_be_eq = core.can_be_eq;
    boolean can_be_lt = core.can_be_lt;
    boolean can_be_gt = core.can_be_gt;

    if (justified() && (can_be_eq || can_be_gt || can_be_lt)) {
      String inequality = (can_be_lt ? "<" : can_be_gt ? ">" : "");
      String comparison = (can_be_eq ? "=" : "");
      if (debugPairwiseIntComparison) {
        System.out.println(repr()
                           + "; inequality=\"" + inequality + "\""
                           + ",comparison=\"" + comparison + "\"");
      }
      return var1().name + " " + inequality + comparison + " " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    int len = Math.min(a1.length, a2.length);
    for (int i=0; i<len; i++) {
      int v1 = a1[i];
      int v2 = a2[i];
      core.add_modified(v1, v2, count);
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

}
