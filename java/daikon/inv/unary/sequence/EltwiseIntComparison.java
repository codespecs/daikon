package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.IntComparisonCore;
import utilMDE.*;
import java.util.*;


// This compares adjacent elements in the sequence.
public class EltwiseIntComparison
  extends SingleSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;

  final static boolean debugEltwiseIntComparison = false;

  public IntComparisonCore core;

  protected EltwiseIntComparison(PptSlice ppt, boolean only_eq) {
    super(ppt);
    core = new IntComparisonCore(this, only_eq);
  }

  public static EltwiseIntComparison instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    // Don't compute ordering relationships over object addresses for
    // elements of a Vector.  (But do compute equality/constant!)
    boolean only_eq = ! ppt.var_infos[0].type.baseIsIntegral();
    return new EltwiseIntComparison(ppt, only_eq);
  }

  protected Object clone() {
    EltwiseIntComparison result = (EltwiseIntComparison) super.clone();
    result.core = (IntComparisonCore) core.clone();
    result.core.wrapper = result;
    return result;
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


  /* IOA */
  public String format_ioa() {
    String[] form =
      VarInfoName.QuantHelper.format_ioa(new VarInfo[] { var(), var() });
    String comparator = core.format_comparator();
    if ("==".equals(comparator)) {
      return form[0] + form[1] + " = " + form[2] + form[3];
    } else {
      return form[0] + "(i+1 = j) => ("+ form[1] + " " + comparator + " " + form[2] + ")" + form[3];
    }
  }

  public String format_esc() {
    String[] form =
      VarInfoName.QuantHelper.format_esc(new VarInfoName[]
	{ var().name, var().name });
    String comparator = core.format_comparator();
    if ("==".equals(comparator)) {
      return form[0] + "(" + form[1] + " == " + form[2] + ")" + form[3];
    } else {
      return form[0] + "((i+1 == j) ==> (" + form[1] + " " + comparator + " " + form[2] + "))" + form[3];
    }
  }

  public String format_simplify() {
    String[] form =
      VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
	{ var().name, var().name });
    String comparator = core.format_comparator();
    if ("==".equals(comparator)) {
      return form[0] + "(EQ"  + " " + form[1] + " " + form[2] + ")" + form[3];
    } else {
      return form[0] + "(IMPLIES (EQ (+ i 1) j) (" + comparator + " " + form[1] + " " + form[2] + "))" + form[3];
    }
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
    if (other instanceof EltwiseIntComparison) {
      return core.isExclusiveFormula(((EltwiseIntComparison) other).core);
    }
    return false;
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
