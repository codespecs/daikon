package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;

import utilMDE.*;

import java.util.*;

// Lexically compares the two sequences.
public class SeqComparison extends TwoSequence implements Comparison {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;

  static Comparator comparator = new ArraysMDE.LongArrayComparatorLexical();

  public final boolean only_check_eq;

  boolean can_be_eq = false;
  boolean can_be_lt = false;
  boolean can_be_gt = false;

  int num_sc_samples = 0;
  private ValueTracker values_cache = new ValueTracker(8);

  protected SeqComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    only_check_eq = only_eq;
  }

  public static SeqComparison instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    // System.out.println("Ppt: " + ppt.name);
    // System.out.println("vars[0]: " + var1.type.format());
    // System.out.println("vars[1]: " + var2.type.format());

    if ((SubSequence.isObviousDerived(var1, var2))
        || (SubSequence.isObviousDerived(var2, var1))) {
      Global.implied_noninstantiated_invariants++;
      return null;
    }

    ProglangType type1 = var1.type;
    ProglangType type2 = var2.type;
    // This intentonally checks dimensions(), not pseudoDimensions.
    boolean only_eq = (! ((type1.dimensions() == 1)
                          && type1.baseIsIntegral()
                          && (type2.dimensions() == 1)
                          && type2.baseIsIntegral()));
    // System.out.println("only_eq: " + only_eq);
    return new SeqComparison(ppt, only_eq);
  }

  protected Object clone() {
    SeqComparison result = (SeqComparison) super.clone();
    result.values_cache = (ValueTracker) values_cache.clone();
    return result;
  }

  protected Invariant resurrect_done_swapped() {
    boolean tmp = can_be_lt;
    can_be_gt = can_be_lt;
    can_be_gt = tmp;
    return this;
  }

  public String repr() {
    return "SeqComparison" + varNames() + ": "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + ",only_check_eq=" + only_check_eq;
  }

  public String format() {
    // System.out.println("Calling SeqComparison.format for: " + repr());
    String comparator = IntComparisonCore.format_comparator(can_be_lt, can_be_eq, can_be_gt);
    return var1().name + " " + comparator + " " + var2().name
      + " (lexically)";
  }

    public String format_java() {
	return var1().name + " " + comparator + " " + var2().name;
    }

  public String format_esc() {
    String classname = this.getClass().toString().substring(6); // remove leading "class"
    return "warning: method " + classname + ".format_esc() needs to be implemented: " + format();
  }

  /* IOA */
  public String format_ioa(String classname) {
    if (var1().isIOASet() || var2().isIOASet())
      return "Not valid for Sets: " + format();
    String comparator = IntComparisonCore.format_comparator(can_be_lt, can_be_eq, can_be_gt);
    comparator = (comparator.equals("==") ? "=" : comparator); // "interned"
    String v1 = var1().name.ioa_name(classname);
    String v2 = var2().name.ioa_name(classname);
    return v1 + " " + comparator + " " + v2 + " ***";
  }

  public String format_simplify() {
    String classname = this.getClass().toString().substring(6); // remove leading "class"
    return "warning: method " + classname + ".format_simplify() needs to be implemented: " + format();
  }

  public void add_modified(long[] v1, long[] v2, int count) {
    /// This does not do the right thing; I really want to avoid comparisons
    /// if one is missing, but not if one is zero-length.
    // Don't make comparisons with empty arrays.
    if ((v1.length == 0) || (v2.length == 0)) {
      return;
    }
    num_sc_samples += count;

    int comparison = comparator.compare(v1, v2);
    // System.out.println("SeqComparison" + varNames() + ": "
    //                    + "compare(" + ArraysMDE.toString(v1)
    //                    + ", " + ArraysMDE.toString(v2) + ") = " + comparison);

    boolean new_can_be_eq = can_be_eq;
    boolean new_can_be_lt = can_be_lt;
    boolean new_can_be_gt = can_be_gt;
    boolean changed = false;
    if (comparison == 0) {
      new_can_be_eq = true;
      changed = true;
    } else if (comparison < 0) {
      new_can_be_lt = true;
      changed = true;
    } else {
      new_can_be_gt = true;
      changed = true;
    }

    if (! changed) {
      values_cache.add(v1, v2);
      return;
    }

    if ((new_can_be_lt && new_can_be_gt)
        || (only_check_eq && (new_can_be_lt || new_can_be_gt))) {
      flowThis();
      destroy();
      return;
    }

    // changed but didn't die
    flowClone();
    can_be_eq = new_can_be_eq;
    can_be_lt = new_can_be_lt;
    can_be_gt = new_can_be_gt;

    values_cache.add(v1, v2);
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else if (can_be_lt || can_be_gt) {
      // System.out.println("prob = " + Math.pow(.5, ppt.num_values()) + " for " + format());
      return Math.pow(.5, values_cache.num_values());
    } else if (num_sc_samples == 0) {
      return Invariant.PROBABILITY_UNJUSTIFIED;
    } else {
      return Invariant.PROBABILITY_JUSTIFIED;
    }
  }

  // For Comparison interface
  public double eq_probability() {
    if (can_be_eq && (!can_be_lt) && (!can_be_gt))
      return computeProbability();
    else
      return Invariant.PROBABILITY_NEVER;
  }

  public boolean isSameFormula(Invariant o)
  {
    SeqComparison other = (SeqComparison) o;
    return
      (can_be_eq == other.can_be_eq) &&
      (can_be_lt == other.can_be_lt) &&
      (can_be_gt == other.can_be_gt);
  }

  public boolean isExclusiveFormula(Invariant o)
  {
    if (o instanceof SeqComparison) {
      SeqComparison other = (SeqComparison) o;
      return (! ((can_be_eq && other.can_be_eq)
                 || (can_be_lt && other.can_be_lt)
                 || (can_be_gt && other.can_be_gt)));
    }
    return false;
  }

  // Copied from IntComparison.
  public boolean isObviousImplied() {
    PairwiseIntComparison pic = PairwiseIntComparison.find(ppt);
    if ((pic != null)
        && (pic.core.can_be_eq == can_be_eq)
        && (pic.core.can_be_lt == can_be_lt)
        && (pic.core.can_be_gt == can_be_gt)) {
      return true;
    }

    return false;
  }

}
