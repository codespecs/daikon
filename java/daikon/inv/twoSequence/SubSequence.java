package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

class SubSequence extends TwoSequence {

  boolean nonsubseq = false;

  SubSequence(PptSlice ppt_) {
    super(ppt_);
  }

  public String repr() {
    double probability = getProbability();
    return "SubSequence" + varNames() + ": "
      + "nonsubseq=" + nonsubseq
      + "; probability = " + probability;
  }

  public String format() {
    if ((!nonsubseq) && justified()) {
      return var1().name + " is a subsequence of " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    if (nonsubseq)
      return;
    nonsubseq = (ArraysMDE.indexOf(a2, a1) == -1);
  }


  protected double computeProbability() {
    if (nonsubseq)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }
}
