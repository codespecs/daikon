package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.IntComparisonCore;
import utilMDE.*;
import java.util.*;


// This compares adjacent elements in the sequence.
public class EltwiseIntComparison extends SingleSequence {

  final static boolean debugEltwiseIntComparison = false;

  public IntComparisonCore core;

  protected EltwiseIntComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    core = new IntComparisonCore(this, only_eq);
  }

  public static EltwiseIntComparison instantiate(PptSlice ppt) {
    // Don't compute ordering relationships over object addresses for
    // elements of a Vector.  (But do compute equality/constant!)
    boolean only_eq = ! ppt.var_infos[0].type.baseIsIntegral();
    return new EltwiseIntComparison(ppt, only_eq);
  }

  public String repr() {
    return "EltwiseIntComparison" + varNames() + ": "
      + core.repr()
      + ",no_invariant=" + no_invariant;
  }

  public String format() {
    if (debugEltwiseIntComparison) {
      System.out.println(repr()
			 + ";comparison=\"" + core.format_comparator() + "\"");
    }
    if (core.can_be_eq && !(core.can_be_lt || core.can_be_gt)) {
      return var().name + " elements are equal";
    } else {
      return (var().name + " sorted by " + core.format_comparator());
    }
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }

  public String format_simplify() {
    return "format_simplify " + this.getClass() + " needs to be changed: " + format();
  }

  public void add_modified(long [] a, int count) {
    for (int i=1; i<a.length; i++) {
      core.add_modified(a[i-1], a[i], count);
      if (no_invariant)
        return;
    }
  }

  // Perhaps check whether all the arrays of interest have length 0 or 1.

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return core.isExact();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((EltwiseIntComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    return core.isExclusiveFormula(((EltwiseIntComparison) other).core);
  }

  // Look up a previously instantiated invariant.
  public static EltwiseIntComparison find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof EltwiseIntComparison)
        return (EltwiseIntComparison) inv;
    }
    return null;
  }



  // Copied from IntComparison.
  // public boolean isExclusiveFormula(Invariant other)
  // {
  //   if (other instanceof IntComparison) {
  //     return core.isExclusiveFormula(((IntComparison) other).core);
  //   }
  //   if (other instanceof NonEqual) {
  //     return isExact();
  //   }
  //   return false;
  // }


  public boolean isObviousImplied() {
    EltOneOf eoo = EltOneOf.find(ppt);
    if ((eoo != null) && eoo.enoughSamples() && (eoo.num_elts() == 1)) {
      return true;
    }
    return false;
  }

}
