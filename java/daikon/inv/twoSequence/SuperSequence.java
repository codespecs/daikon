package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

class SuperSequence extends TwoSequence {

  protected SuperSequence(PptSlice ppt_) {
    super(ppt_);
  }

  public static SuperSequence instantiate(PptSlice ppt) {
    VarInfo subvar = ppt.var_infos[1];
    VarInfo supervar = ppt.var_infos[0];
    // System.out.println("SubSequence.isObviousDerived(" + format() + ") = "
    //                    + SubSequence.isObviousDerived(subvar, supervar));
    if (SubSequence.isObviousDerived(subvar, supervar))
      return null;

    return new SuperSequence(ppt);
  }


  public String repr() {
    double probability = getProbability();
    return "SuperSequence" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    if ((!no_invariant) && justified()) {
      return var2().name + " is a subsequence of " + var1().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    if (ArraysMDE.indexOf(a1, a2) == -1) {
      destroy();
      return;
    }
  }


  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }

}
