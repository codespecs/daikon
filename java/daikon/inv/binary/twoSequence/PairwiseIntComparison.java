package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;
import utilMDE.Assert;
import java.util.Iterator;

// Requires that the lengths are the same.  Determines a comparison that
// holds for all (a[i], b[i]) pairs.


// Also see NonEqual
public class PairwiseIntComparison extends TwoSequence {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;

  final static boolean debugPairwiseIntComparison = false;

  IntComparisonCore core;

  protected PairwiseIntComparison(PptSlice ppt) {
    super(ppt);
    core = new IntComparisonCore(this);
  }

  protected PairwiseIntComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    core = new IntComparisonCore(this, only_eq);
  }

  public static PairwiseIntComparison instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    if ((SubSequence.isObviousDerived(var1, var2))
        || (SubSequence.isObviousDerived(var2, var1))) {
      Global.implied_noninstantiated_invariants++;
      return null;
    }

    boolean only_eq = false;
    if (! (var1.type.elementIsIntegral() && var2.type.elementIsIntegral())) {
      only_eq = true;
    }

    return new PairwiseIntComparison(ppt, only_eq);
  }

  public String repr() {
    return "PairwiseIntComparison" + varNames() + ": "
      + core.repr();
  }

  public String format() {
    String comparator = core.format_comparator();
    return var1().name + " " + comparator + " " + var2().name
      + " (elementwise)";
  }

  /* IOA */
  public String format_ioa(String classname) {
    if (var1().isIOASet() || var2().isIOASet())
      return "Not valid for sets: " + format();
    String comparator = core.format_comparator();
    comparator = comparator.equals("==") ? "=" : comparator;
    String[] form =
      VarInfoName.QuantHelper.format_ioa(new VarInfo[] { var1(), var2() }, classname);
    return form[0]+"("+form[4]+"="+form[5]+") => ("+form[1]+" "+comparator+" "+form[2]+")"+form[3];
  }

  public String format_esc() {
    String comparator = core.format_comparator();
    String[] form =
      VarInfoName.QuantHelper.format_esc(new VarInfoName[]
	{ var1().name, var2().name }, true); // elementwise
    return form[0] + "(" + form[1] + " " + comparator + " " + form[2] + ")" + form[3];
  }

  public String format_simplify() {
    String comparator = core.format_comparator();
    if ("==".equals(comparator)) {
      comparator = "EQ";
    }
    String[] form =
      VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
	{ var1().name, var2().name }, true); // elementwise
    return form[0] + "(" + comparator + " " + form[1] + " " + form[2] + ")" + form[3];
  }

  public void add_modified(long[] a1, long[] a2, int count) {
    if (a1.length != a2.length) {
      destroy();
      return;
    }
    int len = a1.length;
    // int len = Math.min(a1.length, a2.length);

    for (int i=0; i<len; i++) {
      long v1 = a1[i];
      long v2 = a2[i];
      core.add_modified(v1, v2, count);
      if (no_invariant)
        return;
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((PairwiseIntComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof PairwiseIntComparison) {
      return core.isExclusiveFormula(((PairwiseIntComparison) other).core);
    }
    return false;
  }

  // Look up a previously instantiated invariant.
  public static PairwiseIntComparison find(PptSlice ppt) {
    Assert.assert(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof PairwiseIntComparison)
        return (PairwiseIntComparison) inv;
    }
    return null;
  }

}
