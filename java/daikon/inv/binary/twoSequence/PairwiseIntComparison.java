package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;

// Requires that the lengths are the same.  Should it?  (Hard to tell;
// could argue either way, on completeness vs. efficiency grounds.)

// Also see NonEqual
public class PairwiseIntComparison extends TwoSequence {

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
    return "PairwiseIntComparison" + varNames() + ": "
      + core.repr();
  }

  public String format() {
    String comparator = core.format_comparator();
    return var1().name + " " + comparator + " " + var2().name
      + " (elementwise)";
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }


  public void add_modified(long[] a1, long[] a2, int count) {
    if (a1.length != a2.length) {
      destroy();
      return;
    }
    int len = a1.length;
    // int len = Math.min(a1.length, a2.length);

    for (int i=0; i<len; i++) {
      long v1 = a1[i];
      long v2 = a2[i];
      core.add_modified(v1, v2, count);
      if (no_invariant)
        return;
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((PairwiseIntComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof PairwiseIntComparison) {
      return core.isExclusiveFormula(((PairwiseIntComparison) other).core);
    }
    return false;
  }

}
