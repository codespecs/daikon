package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

// similar to SuperSequence; if I change one, also change the other.
class SubSequence extends TwoSequence {

  protected SubSequence(PptSlice ppt) {
    super(ppt);
  }

  public static SubSequence instantiate(PptSlice ppt) {
    VarInfo subvar = ppt.var_infos[0];
    VarInfo supervar = ppt.var_infos[1];
    // System.out.println("SubSequence.isObviousDerived(" + format() + ") = "
    //                    + SubSequence.isObviousDerived(subvar, supervar));
    if (SubSequence.isObviousDerived(subvar, supervar)) {
      Global.implied_noninstantiated_invariants++;
      return null;
    }

    return new SubSequence(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "SubSequence" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    return var1().name + " is a subsequence of " + var2().name;
  }


  public void add_modified(long[] a1, long[] a2, int count) {
    if (ArraysMDE.indexOf(a2, a1) == -1) {
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

  // This is abstracted out so it can be called by SuperSequence as well.
  static boolean isObviousDerived(VarInfo subvar, VarInfo supervar) {
    // System.out.println("static SubSequence.isObviousDerived(" + subvar.name + ", " + supervar.name + ") " + subvar.isDerivedSubSequenceOf() + " " + supervar.isDerivedSubSequenceOf());

    VarInfo subvar_super = subvar.isDerivedSubSequenceOf();
    if (subvar_super == null)
      return false;

    if ((subvar_super == supervar)
        // whatever we come up with, it will be obvious from the
        // IntComparison relationship over the lengths.
        || (subvar_super == supervar.isDerivedSubSequenceOf()))
      return true;

    return false;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof SubSequence);
    return true;
  }

}
