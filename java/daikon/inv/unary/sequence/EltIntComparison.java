package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

// I should perhaps reimplement this, using IntComparisonCore.


// This compares adjacent elements in the sequence.
class EltIntComparison extends SingleSequence {

  final static boolean debugEltIntComparison = false;

  public final boolean only_check_eq;

  boolean can_be_eq = false;
  boolean can_be_lt = false;
  boolean can_be_gt = false;

  protected EltIntComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    only_check_eq = only_eq;
  }

  public static EltIntComparison instantiate(PptSlice ppt) {
    // Don't compute ordering relationships over object addresses for
    // elements of a Vector.  (But do compute equality/constant!)
    boolean only_eq = ! ppt.var_infos[0].type.baseIsIntegral();
    return new EltIntComparison(ppt, only_eq);
  }

  public String repr() {
    double probability = getProbability();
    return "EltIntComparison(" + var().name + "): "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + ",only_check_eq=" + only_check_eq
      + "; probability = " + probability;
  }

  public String format() {
    if (justified() && (can_be_eq || can_be_gt || can_be_lt)) {
      String inequality = (can_be_lt ? "<" : can_be_gt ? ">" : "");
      String comparison = (can_be_eq ? "=" : "");
      if (debugEltIntComparison) {
        System.out.println(repr()
                           + "; inequality=\"" + inequality + "\""
                           + ",comparison=\"" + comparison + "\"");
      }
      return (var().name + " sorted by "
              + inequality + comparison);
    } else {
      return null;
    }
  }


  public void add_modified(long[] a, int count) {
    for (int i=1; i<a.length; i++) {
      long v1 = a[i-1];
      long v2 = a[i];
      if (v1 == v2)
        can_be_eq = true;
      else if (v1 < v2)
        can_be_lt = true;
      else
        can_be_gt = true;
    }
    if ((can_be_lt && can_be_gt)
        || (only_check_eq && (can_be_lt || can_be_gt))) {
      destroy();
      return;
    }
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else if (! (can_be_lt || can_be_gt || can_be_eq)) {
      // This can happen if all the arrays of interest have
      // length 0 or 1.  (Mabye I should check that.)
      return Invariant.PROBABILITY_UNJUSTIFIED;
    } else if (can_be_lt || can_be_gt) {
      return Math.pow(.5, ppt.num_values());
    } else {
      Assert.assert(can_be_eq);
      return Invariant.PROBABILITY_JUSTIFIED;
    }
  }

  public boolean isSameFormula(Invariant o)
  {
    EltIntComparison other = (EltIntComparison) o;
    return
      (can_be_eq == other.can_be_eq) &&
      (can_be_gt == other.can_be_gt) &&
      (can_be_lt == other.can_be_lt);
  }
  
}
