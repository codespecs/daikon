package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.sequenceScalar.*;
import daikon.derive.*;
import daikon.derive.unary.*;


// Also see NonEqual, NonAliased
class IntComparison extends TwoScalar implements Comparison {

  final static boolean debugIntComparison = false;

  IntComparisonCore core;

  protected IntComparison(PptSlice ppt_) {
    this(ppt_, false, false, false, false);
  }

  protected IntComparison(PptSlice ppt_, boolean obvious_lt, boolean obvious_gt, boolean obvious_le, boolean obvious_ge) {
    super(ppt_);
    core = new IntComparisonCore(this, obvious_lt, obvious_gt, obvious_le, obvious_ge);
  }

  public static IntComparison instantiate(PptSlice ppt) {
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo seqvar1 = var1.isDerivedSequenceMember();
    VarInfo seqvar2 = var2.isDerivedSequenceMember();

    if (debugIntComparison) {
      System.out.println("IntComparison.instantiate(" + ppt.name + ")"
                         + ", seqvar1=" + seqvar1
                         + ", seqvar2=" + seqvar2);
    }

    // Yes, this comparison is obvious.  However:
    //  * we don't know whether it is an equality or a non-strict inequality.
    //  * we may wish to have the comparison on hand later, when we iterate
    //    over all IntComparison objects.
    // I should suppress this on *output*, not on computation.
    // Better yet:  suppress it as soon as it becomes obvious.
    // if ((seqvar1 != null) && (seqvar1 == var2.isDerivedSequenceMember())) {
    //   if ((var1.derived instanceof SequenceMax)
    //       || (var1.derived instanceof SequenceMin)
    //       || (var2.derived instanceof SequenceMax)
    //       || (var2.derived instanceof SequenceMin)) {
    //     return null;
    //   }
    // }

    boolean obvious_lt = false;
    boolean obvious_gt = false;
    boolean obvious_le = false;
    boolean obvious_ge = false;
    Derivation deriv1 = var1.derived;
    Derivation deriv2 = var2.derived;
    if ((seqvar1 != null) && (seqvar2 != null)) {
      boolean min1 = (deriv1 instanceof SequenceMin);
      boolean max1 = (deriv1 instanceof SequenceMax);
      boolean min2 = (deriv2 instanceof SequenceMin);
      boolean max2 = (deriv2 instanceof SequenceMax);
      VarInfo super1 = seqvar1.isDerivedSubSequenceOf();
      VarInfo super2 = seqvar2.isDerivedSubSequenceOf();

      if (debugIntComparison) {
        System.out.println("IntComparison.instantiate: "
                           + "min1=" + min1
                           + ", max1=" + max1
                           + ", min2=" + min2
                           + ", max2=" + max2
                           + ", super1=" + super1
                           + ", super2=" + super2
                           + ", iom(var2, seqvar1)=" + Member.isObviousMember(var2, seqvar1)
                           + ", iom(var1, seqvar2)=" + Member.isObviousMember(var1, seqvar2));
      }
      if (seqvar1 == seqvar2) {
        // Same sequence.  The invariant is obvious as soon as it's nonequal,
        // because "all elements equal" will be reported elsewhere.
        if (min1 || max2)
          obvious_lt = true;
        else if (max1 || min2)
          obvious_gt = true;
      } else if ((min1 || max1) && Member.isObviousMember(var2, seqvar1)) {
        if (min1) {
          obvious_le = true;
        } else if (max1) {
          obvious_ge = true;
        }
      } else if ((min2 || max2) && Member.isObviousMember(var1, seqvar2)) {
        if (min2) {
          obvious_ge = true;
        } else if (max2) {
          obvious_le = true;
        }
      }

    }

    return new IntComparison(ppt, obvious_lt, obvious_gt, obvious_le, obvious_ge);

  }


  // public boolean isObviousDerived() {
  //   boolean can_be_eq = core.can_be_eq;
  //   boolean can_be_lt = core.can_be_lt;
  //   boolean can_be_gt = core.can_be_gt;

  //   VarInfo var1 = ppt.var_infos[0];
  //   VarInfo var2 = ppt.var_infos[1];
  //   VarInfo seqvar1 = var1.isDerivedSequenceMember();
  //   if (debugIntComparison)
  //     System.out.println("IntComparison.isObviousDerived(" + ppt.name + ")"
  //                        + ", seqvar1=" + seqvar1
  //                        + ", seqvar2=" + var2.isDerivedSequenceMember());
  //   if ((seqvar1 != null) && (seqvar1 == var2.isDerivedSequenceMember())) {
  //     if (((var1.derived instanceof SequenceMin)
  //          && (var2.derived instanceof SequenceMax))
  //         || ((var1.derived instanceof SequenceMax)
  //             && (var2.derived instanceof SequenceMin)))
  //       return true;
  //     if ((var1.derived instanceof SequenceMin)
  //         && can_be_eq && can_be_lt) {
  //       return true;
  //     } else if ((var1.derived instanceof SequenceMax)
  //                && can_be_eq && can_be_gt) {
  //       return true;
  //     } else if ((var2.derived instanceof SequenceMin)
  //                && can_be_eq && can_be_gt) {
  //       return true;
  //     } else if ((var2.derived instanceof SequenceMax)
  //                && can_be_eq && can_be_lt) {
  //       return true;
  //     }
  //   }
  //   return false;
  // }


  public String repr() {
    boolean can_be_eq = core.can_be_eq;
    boolean can_be_lt = core.can_be_lt;
    boolean can_be_gt = core.can_be_gt;
    boolean obvious_can_be_lt = core.can_be_lt;
    boolean obvious_can_be_gt = core.can_be_gt;

    double probability = getProbability();
    return "IntComparison(" + var1().name + "," + var2().name + "): "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + ",obvious_can_be_lt=" + obvious_can_be_lt
      + ",obvious_can_be_gt=" + obvious_can_be_gt
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
