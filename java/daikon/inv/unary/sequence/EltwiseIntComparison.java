package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

// I should reimplement this, using IntComparisonCore.


// This compares adjacent elements in the sequence.
public class EltwiseIntComparison extends SingleSequence {

  final static boolean debugEltwiseIntComparison = false;

  public final boolean only_check_eq;

  boolean can_be_eq = false;
  boolean can_be_lt = false;
  boolean can_be_gt = false;

  protected EltwiseIntComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    only_check_eq = only_eq;
  }

  public static EltwiseIntComparison instantiate(PptSlice ppt) {
    // Don't compute ordering relationships over object addresses for
    // elements of a Vector.  (But do compute equality/constant!)
    boolean only_eq = ! ppt.var_infos[0].type.baseIsIntegral();
    return new EltwiseIntComparison(ppt, only_eq);
  }

  public String repr() {
    return "EltwiseIntComparison" + varNames() + ": "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + ",only_check_eq=" + only_check_eq;
  }

  public String format() {
    // Don't do this; format() shouldn't check justification (in general...).
    // Assert.assert(!justified() || can_be_eq || can_be_lt || can_be_gt);
    String inequality = (can_be_lt ? "<" : can_be_gt ? ">" : "");
    String comparison = (can_be_eq ? "=" : "");
    if (debugEltwiseIntComparison) {
      System.out.println(repr()
			 + "; inequality=\"" + inequality + "\""
			 + ",comparison=\"" + comparison + "\"");
    }
    if (can_be_eq && !(can_be_lt || can_be_gt)) {
      return var().name + " elements are equal";
    } else {
      return (var().name + " sorted by " + inequality + comparison);
    }
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
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
    EltwiseIntComparison other = (EltwiseIntComparison) o;
    return
      (can_be_eq == other.can_be_eq) &&
      (can_be_gt == other.can_be_gt) &&
      (can_be_lt == other.can_be_lt);
  }

  public boolean isExclusiveFormula(Invariant o)
  {
    if (o instanceof EltwiseIntComparison) {
      EltwiseIntComparison other = (EltwiseIntComparison) o;
      return (! ((can_be_eq && other.can_be_eq)
                 || (can_be_gt && other.can_be_gt)
                 || (can_be_lt && other.can_be_lt)));
    }
    return false;
  }

  public boolean isObviousImplied() {
    EltOneOf eoo = EltOneOf.find(ppt);
    if ((eoo != null) && eoo.justified() && (eoo.num_elts() == 1)) {
      return true;
    }
    return false;
  }

}
