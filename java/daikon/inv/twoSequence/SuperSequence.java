package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

class SuperSequence extends TwoSequence {

  boolean nonsuperseq = false;

  SuperSequence(PptSlice ppt_) {
    super(ppt_);
  }

  public String repr() {
    double probability = getProbability();
    return "SuperSequence" + varNames() + ": "
      + "nonsuperseq=" + nonsuperseq
      + "; probability = " + probability;
  }

  public String format() {
    if ((!nonsuperseq) && justified()) {
      return var2().name + " is a subsequence of " + var1().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    if (nonsuperseq)
      return;
    nonsuperseq = (ArraysMDE.indexOf(a1, a2) == -1);
  }


  protected double computeProbability() {
    if (nonsuperseq)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }
}
