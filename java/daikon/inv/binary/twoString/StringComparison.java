package daikon.inv.binary.twoString;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.sequenceString.*;
import daikon.derive.*;
import daikon.derive.unary.*;

import utilMDE.*;

import java.util.*;

/**
 * Represents lexical invariants between two strings.  Prints as
 * "s1 [cmp] s2" where [cmp] is one of == > >= < <=
 **/

public final class StringComparison
  extends TwoString
  implements Comparison
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff StringComparison invariants should be considered.
   **/
  public static boolean dkconfig_enabled = true;

  final static boolean debugStringComparison = false;

  public StringComparisonCore core;

  protected StringComparison(PptSlice ppt) {
    this(ppt, false, false, false, false, false);
  }

  protected StringComparison(PptSlice ppt, boolean only_eq, boolean obvious_lt, boolean obvious_gt, boolean obvious_le, boolean obvious_ge) {
    super(ppt);
    core = new StringComparisonCore(this, only_eq, obvious_lt, obvious_gt, obvious_le, obvious_ge);
  }

  public static StringComparison instantiate(PptSlice ppt) {
    return instantiate (ppt, false);
  }

  public static StringComparison instantiate(PptSlice ppt, boolean onlyEq) {
    if (!dkconfig_enabled) return null;

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo seqvar1 = var1.isDerivedSequenceMember();
    VarInfo seqvar2 = var2.isDerivedSequenceMember();

    if (debugStringComparison || ppt.debugged) {
      System.out.println("StringComparison.instantiate(" + ppt.name + ")"
                         + ", seqvar1=" + seqvar1
                         + ", seqvar2=" + seqvar2);
    }

    boolean only_eq = onlyEq;
    boolean obvious_lt = false;
    boolean obvious_gt = false;
    boolean obvious_le = false;
    boolean obvious_ge = false;

    if (! (var1.type.isIntegral() && var2.type.isIntegral())) {
      only_eq = true;
    } else {
      if ((seqvar1 != null) && (seqvar2 != null)) {
        Derivation deriv1 = var1.derived;
        Derivation deriv2 = var2.derived;
        boolean min1 = (deriv1 instanceof SequenceMin);
        boolean max1 = (deriv1 instanceof SequenceMax);
        boolean min2 = (deriv2 instanceof SequenceMin);
        boolean max2 = (deriv2 instanceof SequenceMax);
        VarInfo super1 = seqvar1.isDerivedSubSequenceOf();
        VarInfo super2 = seqvar2.isDerivedSubSequenceOf();

        if (debugStringComparison || ppt.debugged) {
          System.out.println("StringComparison.instantiate: "
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
        }      }
    }

    return new StringComparison(ppt, only_eq, obvious_lt, obvious_gt, obvious_le, obvious_ge);

  }

  protected Object clone() {
    StringComparison result = (StringComparison) super.clone();
    result.core = (StringComparisonCore) core.clone();
    result.core.wrapper = result;
    return result;
  }

  protected Invariant resurrect_done_swapped() {
    core.swap();
    return this;
  }

  // Look up a previously instantiated StringComparison relationship.
  // Should this implementation be made more efficient?
  public static StringComparison find(PptSlice ppt) {
    Assert.assertTrue(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof StringComparison)
        return (StringComparison) inv;
    }
    return null;
  }

  public String repr() {
    return "StringComparison" + varNames() + ": " + core.repr();
  }

  public String format_using(OutputFormat format) {
    String name1;
    String name2;
    String comparator = core.format_comparator(format);

    if (format == OutputFormat.JAVA || format == OutputFormat.JML) {
      name1 = var1().name.JMLElementCorrector().name_using(format);
      name2 = var2().name.JMLElementCorrector().name_using(format);
      if (comparator.equals("==")) {
        return name1 + ".equals(" + name2 + ")";
      } else {
        return name1 + ".compareTo(" + name2 + ") " + comparator + " 0";
      }
    } else {
      name1 = var1().name.name_using(format);
      name2 = var2().name.name_using(format);
      return name1 + " " + comparator + " " + name2;
    }
    // return format_unimplemented(format);
  }

  public void add_modified(String v1, String v2, int count) {
    if (ppt.debugged) {
      System.out.println("StringComparison" + ppt.varNames() + ".add_modified("
                         + v1 + "," + v2 + ", count=" + count + ")");
    }
    core.add_modified(v1, v2, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  // For Comparison interface
  public double eq_probability() {
    if (isExact())
      return computeProbability();
    else
      return Invariant.PROBABILITY_NEVER;
  }

  public boolean isExact() {
    return core.isExact();
  }

  // // Temporary, for debugging
  // public void destroy() {
  //   if (debugStringComparison || ppt.debugged) {
  //     System.out.println("StringComparison.destroy(" + ppt.name + ")");
  //   }
  //   super.destroy();
  // }

  public void add(String v1, String v2, int mod_index, int count) {
    if (ppt.debugged) {
      System.out.println("StringComparison" + ppt.varNames() + ".add("
                         + v1 + "," + v2
                         + ", mod_index=" + mod_index + ")"
                         + ", count=" + count + ")");
    }
    super.add(v1, v2, mod_index, count);
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((StringComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof StringComparison) {
      return core.isExclusiveFormula(((StringComparison) other).core);
    }
    return false;
  }

  /**
   *  Since this invariant can be a postProcessed equality, we have to
   *  handle isObvious especially to avoid circular isObvious
   *  relations.
   **/
  public VarInfo[] isObviousStatically_SomeInEquality() {
    if (var1().equalitySet == var2().equalitySet) {
      if (isObviousStatically (this.ppt.var_infos)) {
        return this.ppt.var_infos;
      } else {
        return null;
      }
    } else {
      return super.isObviousStatically_SomeInEquality();
    }
  }

  /**
   *  Since this invariant can be a postProcessed equality, we have to
   *  handle isObvious especially to avoid circular isObvious
   *  relations.
   **/
  public VarInfo[] isObviousDynamically_SomeInEquality() {
    if (var1().equalitySet == var2().equalitySet) {
      if (isObviousDynamically (this.ppt.var_infos)) {
        return this.ppt.var_infos;
      } else {
        return null;
      }
    } else {
      return super.isObviousDynamically_SomeInEquality();
    }
  }

}
