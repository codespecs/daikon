package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

// similar to SubSequence; if I change one, also change the other.
class SuperSequence extends TwoSequence {

  protected SuperSequence(PptSlice ppt) {
    super(ppt);
  }

  public static SuperSequence instantiate(PptSlice ppt) {
    VarInfo subvar = ppt.var_infos[1];
    VarInfo supervar = ppt.var_infos[0];
    // System.out.println("SubSequence.isObviousDerived(" + format() + ") = "
    //                    + SubSequence.isObviousDerived(subvar, supervar));
    if (SubSequence.isObviousDerived(subvar, supervar)) {
      Global.implied_noninstantiated_invariants++;
      return null;
    }

    return new SuperSequence(ppt);
  }


  public String repr() {
    double probability = getProbability();
    return "SuperSequence" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    return var2().name + " is a subsequence of " + var1().name;
  }


  public void add_modified(long[] a1, long[] a2, int count) {
    if (ArraysMDE.indexOf(a1, a2) == -1) {
      destroy();
      return;
    }
  }


  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else
      return Invariant.PROBABILITY_JUSTIFIED;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof SuperSequence);
    return true;
  }

}
