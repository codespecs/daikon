package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

class SubSequence extends TwoSequence {

  protected SubSequence(PptSlice ppt_) {
    super(ppt_);
  }

  public static SubSequence instantiate(PptSlice ppt) {
    VarInfo subvar = ppt.var_infos[0];
    VarInfo supervar = ppt.var_infos[1];
    // System.out.println("SubSequence.isObviousDerived(" + format() + ") = "
    //                    + SubSequence.isObviousDerived(subvar, supervar));
    if (SubSequence.isObviousDerived(subvar, supervar))
      return null;

    return new SubSequence(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "SubSequence" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    if ((!no_invariant) && justified()) {
      return var1().name + " is a subsequence of " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a1, int[] a2, int count) {
    no_invariant = (ArraysMDE.indexOf(a2, a1) == -1);
  }


  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }

  // This is abstracted out so it can be called by SuperSequence as well.
  static boolean isObviousDerived(VarInfo subvar, VarInfo supervar) {
    // System.out.println("static SubSequence.isObviousDerived(" + subvar.name + ", " + supervar.name + ") " + subvar.isObviousSubSequenceOf() + " " + supervar.isObviousSubSequenceOf());

    VarInfo subvar_super = subvar.isObviousSubSequenceOf();
    if (subvar_super == null)
      return false;

    if ((subvar_super == supervar)
        // whatever we come up with, it will be obvious from the
        // IntComparison relationship over the lengths.
        || (subvar_super == supervar.isObviousSubSequenceOf()))
      return true;

    return false;
  }

}
