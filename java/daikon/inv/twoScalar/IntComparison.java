package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;
import daikon.derive.unary.*;


// Also see NonEqual, NonAliased
class IntComparison extends TwoScalar implements Comparison {

  final static boolean debugIntComparison = false;

  IntComparisonCore core;

  protected IntComparison(PptSlice ppt_) {
    super(ppt_);
    core = new IntComparisonCore(this);
  }

  public static IntComparison instantiate(PptSlice ppt) {
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo seqvar1 = var1.isObviousSequenceMember();
    // System.out.println("IntComparison.instantiate(" + ppt.name + ")"
    //                    + ", seqvar1=" + seqvar1
    //                    + ", seqvar2=" + var2.isObviousSequenceMember());
    if ((seqvar1 != null) && (seqvar1 == var2.isObviousSequenceMember())) {
      if ((var1.derived instanceof SequenceMax)
          || (var1.derived instanceof SequenceMin)
          || (var2.derived instanceof SequenceMax)
          || (var2.derived instanceof SequenceMin)) {
        return null;
      }
    }

    return new IntComparison(ppt);
  }

  public String repr() {
    boolean can_be_eq = core.can_be_eq;
    boolean can_be_lt = core.can_be_lt;
    boolean can_be_gt = core.can_be_gt;

    double probability = getProbability();
    return "IntComparison(" + var1().name + "," + var2().name + "): "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + "; probability = " + probability;
  }

  public String format() {
    boolean can_be_eq = core.can_be_eq;
    boolean can_be_lt = core.can_be_lt;
    boolean can_be_gt = core.can_be_gt;

    if (justified() && (can_be_eq || can_be_gt || can_be_lt)) {
      String inequality = (can_be_lt ? "<" : can_be_gt ? ">" : "");
      String comparison = (can_be_eq ? "=" : "");
      if (debugIntComparison) {
        System.out.println(repr()
                           + "; inequality=\"" + inequality + "\""
                           + ",comparison=\"" + comparison + "\"");
      }
      return var1().name + " " + inequality + comparison + " " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int v1, int v2, int count) {
    core.add_modified(v1, v2, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  // For Comparison interface
  public double eq_probability() {
    boolean can_be_eq = core.can_be_eq;
    boolean can_be_lt = core.can_be_lt;
    boolean can_be_gt = core.can_be_gt;

    if (can_be_eq && (!can_be_lt) && (!can_be_gt))
      return computeProbability();
    else
      return Invariant.PROBABILITY_NEVER;
  }

}



