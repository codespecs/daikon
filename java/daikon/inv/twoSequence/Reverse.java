package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

class Reverse extends TwoSequence {

  boolean nonreverse = false;

  Reverse(PptSlice ppt_) {
    super(ppt_);
  }

  public String repr() {
    double probability = getProbability();
    return "Reverse" + varNames() + ": "
      + "nonreverse=" + nonreverse
      + "; probability = " + probability;
  }

  public String format() {
    if ((!nonreverse) && justified()) {
      return var1().name + " is the reverse of " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    if (nonreverse)
      return;
    if (a1.length != a2.length) {
      nonreverse = true;
      return;
    }
    int len = a1.length;
    for (int i=0, j=len-1; i<len; i++, j--)
      if (a1[i] != a2[j]) {
        nonreverse = true;
        return;
      }
  }


  protected double computeProbability() {
    if (nonreverse)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }
}
