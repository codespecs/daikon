package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.sequenceScalar.*;
import daikon.derive.*;
import daikon.derive.unary.*;

import utilMDE.*;

import java.util.*;

// Also see NonEqual
public final class IntComparison extends TwoScalar implements Comparison {

  final static boolean debugIntComparison = false;

  public IntComparisonCore core;

  protected IntComparison(PptSlice ppt) {
    this(ppt, false, false, false, false, false);
  }

  protected IntComparison(PptSlice ppt, boolean only_eq, boolean obvious_lt, boolean obvious_gt, boolean obvious_le, boolean obvious_ge) {
    super(ppt);
    core = new IntComparisonCore(this, only_eq, obvious_lt, obvious_gt, obvious_le, obvious_ge);
  }

  public static IntComparison instantiate(PptSlice ppt) {
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    VarInfo seqvar1 = var1.isDerivedSequenceMember();
    VarInfo seqvar2 = var2.isDerivedSequenceMember();

    if (debugIntComparison || ppt.debugged) {
      System.out.println("IntComparison.instantiate(" + ppt.name + ")"
                         + ", seqvar1=" + seqvar1
                         + ", seqvar2=" + seqvar2);
    }

    boolean only_eq = false;
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

        if (debugIntComparison || ppt.debugged) {
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
        } else if (((min1 && max2) || (max1 && min2))
                   && (super1 != null) && (super2 != null) && (super1 == super2)
                   && VarInfo.seqs_overlap(seqvar1, seqvar2)) {
          // If the sequences overlap, then clearly the min of either is no
          // greater than the max of the other.
          if (min1 && max2) {
            obvious_le = true;
            // System.out.println("obvious_le: " + var1.name + " " + var2.name);
          } else if (max1 && min2) {
            obvious_ge = true;
            // System.out.println("obvious_ge: " + var1.name + " " + var2.name);
          }
        }
      }
    }

    return new IntComparison(ppt, only_eq, obvious_lt, obvious_gt, obvious_le, obvious_ge);

  }

  // Look up a previously instantiated IntComparison relationship.
  // Should this implementation be made more efficient?
  public static IntComparison find(PptSlice ppt) {
    Assert.assert(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof IntComparison)
        return (IntComparison) inv;
    }
    return null;
  }



  public String repr() {
    return "IntComparison" + varNames() + ": "
      + core.repr();
  }

  public String format() {
    String comparator = core.format_comparator();
    return var1().name + " " + comparator + " " + var2().name;
  }

  public String format_esc() {
    String comparator = core.format_comparator();
    return var1().esc_name + " " + comparator + " " + var2().esc_name;
  }


  public void add_modified(long v1, long v2, int count) {
    if (ppt.debugged) {
      System.out.println("IntComparison" + ppt.varNames() + ".add_modified("
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
  //   if (debugIntComparison || ppt.debugged) {
  //     System.out.println("IntComparison.destroy(" + ppt.name + ")");
  //   }
  //   super.destroy();
  // }

  public void add(long v1, long v2, int mod_index, int count) {
    if (ppt.debugged) {
      System.out.println("IntComparison" + ppt.varNames() + ".add("
                         + v1 + "," + v2
                         + ", mod_index=" + mod_index + ")"
                         + ", count=" + count + ")");
    }
    super.add(v1, v2, mod_index, count);
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((IntComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof IntComparison) {
      return core.isExclusiveFormula(((IntComparison) other).core);
    }
    if (other instanceof NonEqual) {
      return isExact();
    }

    return false;
  }


  public boolean isObviousImplied() {
    if (isExact()) {
      return false;
    }
    LinearBinary lb = LinearBinary.find(ppt);
    if ((lb != null) && (lb.core.a == 1) && lb.justified()) {
      Assert.assert(lb.core.b != 0);
      return true;
    }
    { // Sequence length tests
      VarInfo var1 = ppt.var_infos[0];
      VarInfo var2 = ppt.var_infos[1];
      SequenceLength sl1 = null;
      if (var1.isDerived() && (var1.derived instanceof SequenceLength))
        sl1 = (SequenceLength) var1.derived;
      SequenceLength sl2 = null;
      if (var2.isDerived() && (var2.derived instanceof SequenceLength))
        sl2 = (SequenceLength) var2.derived;
      if ((sl1 != null) && (sl2 != null)
          && ((sl1.shift == sl2.shift) && (sl1.shift != 0) || (sl2.shift != 0))) {
        // "size(a)-1 cmp size(b)-1"; should just use "size(a) cmp size(b)"
        return true;
      }

      // This might never get invoked, as equality is printed out specially.
      VarInfo s1 = (sl1 == null) ? null : sl1.base;
      VarInfo s2 = (sl2 == null) ? null : sl2.base;
      if ((s1 != null) && (s2 != null)
          && (s1.equal_to == s2.equal_to)) {
        // lengths of equal arrays being compared
        return true;
      }

      if (core.can_be_lt && (!core.can_be_eq)) {
        if ((sl2 != null) && (sl2.shift == 0)) {
          // "x < size(a)"  ("x <= size(a)-1" or "x < size(a)-1" would be more informative)
          return true;
        } else if ((sl1 != null) && (sl1.shift == -1)) {
          // "size(a)-1 < x"  ("size(a) <= x" would be more informative)
          return true;
        }
      } else if (core.can_be_gt && (!core.can_be_eq)) {
        if ((sl1 != null) && (sl1.shift == 0)) {
          // "size(a) > x"  ("size(a) >= x" would be more informative)
          return true;
        } else if ((sl2 != null) && (sl2.shift == -1)) {
          // "x > size(a)-1"  ("x >= size(a)" would be more informative)
          return true;
        }
      }
    }

    return false;
  }

}
