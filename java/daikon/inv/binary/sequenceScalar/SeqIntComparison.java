package daikon.inv.binary.sequenceScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import java.util.*;
import utilMDE.*;

// I should perhaps reimplement this, using IntComparisonCore.

public final class SeqIntComparison extends SequenceScalar {

  public IntComparisonCore core;

  static boolean debugSeqIntComparison = false;

  protected SeqIntComparison(PptSlice ppt, boolean seq_first, boolean only_eq) {
    super(ppt, seq_first);
    Assert.assert(sclvar().rep_type == ProglangType.INT );
    Assert.assert(seqvar().rep_type == ProglangType.INT_ARRAY );
    core = new IntComparisonCore(this, only_eq);
  }

  public static SeqIntComparison instantiate(PptSlice ppt, boolean seq_first) {
    VarInfo seqvar = ppt.var_infos[seq_first ? 0 : 1];
    VarInfo sclvar = ppt.var_infos[seq_first ? 1 : 0];

    // if (isEqualToObviousSeqIntComparison(sclvar, seqvar)) {
    //   Global.implied_noninstantiated_invariants += 1;
    //   if (debugSeqIntComparison) {
    //     System.out.println("SeqIntComparison not instantiated (obvious): "
    //                        + sclvar.name + " in " + seqvar.name);
    //   }
    //   return null;
    // }

    if (debugSeqIntComparison) {
      System.out.println("SeqIntComparison instantiated: "
                         + sclvar.name + " in " + seqvar.name);
    }

    // Don't compute ordering relationships over object addresses for
    // elements of a Vector.  (But do compute equality/constant!)
    ProglangType elt_type = seqvar.type.elementType();
    Assert.assert(elt_type == sclvar.type);
    boolean only_eq = ! elt_type.baseIsIntegral();
    return new SeqIntComparison(ppt, seq_first, only_eq);
  }

  // public boolean isObviousImplied() {
  //   return isEqualToObviousSeqIntComparison(sclvar(), seqvar());
  // }

  public String repr() {
    return "SeqIntComparison" + varNames() + ": "
      + core.repr()
      + ",no_invariant=" + no_invariant;
  }

  public String format() {
    // Don't do this; format() shouldn't check justification (in general...).
    // Assert.assert(!justified() || can_be_eq || can_be_lt || can_be_gt);
    String comparator = core.format_comparator();
    return seqvar().name + " elements " + comparator + " " + sclvar().name;
  }

  public String format_esc() {
    String comparator = core.format_comparator();
    String[] esc_forall = seqvar().esc_forall();
    return "(" + esc_forall[0]
      + "(" + esc_forall[1] + " " + comparator + " " + sclvar().name + "))";
  }

  public void add_modified(long [] a, long x, int count) {
    for (int i=1; i<a.length; i++) {
      core.add_modified(a[i], x, count);
      if (no_invariant)
        return;
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return core.isExact();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((SeqIntComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    return false;
  }

  // Look up a previously instantiated invariant.
  public static SeqIntComparison find(PptSlice ppt) {
    Assert.assert(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof SeqIntComparison)
        return (SeqIntComparison) inv;
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


  // Copied from IntComparison.
  // public boolean isObviousImplied() {
  //   if (isExact()) {
  //     return false;
  //   }
  //   LinearBinary lb = LinearBinary.find(ppt);
  //   if ((lb != null) && (lb.core.a == 1) && lb.justified()) {
  //     Assert.assert(lb.core.b != 0);
  //     return true;
  //   }
  //   { // Sequence length tests
  //     VarInfo var1 = ppt.var_infos[0];
  //     VarInfo var2 = ppt.var_infos[1];
  //     SequenceLength sl1 = null;
  //     if (var1.isDerived() && (var1.derived instanceof SequenceLength))
  //       sl1 = (SequenceLength) var1.derived;
  //     SequenceLength sl2 = null;
  //     if (var2.isDerived() && (var2.derived instanceof SequenceLength))
  //       sl2 = (SequenceLength) var2.derived;
  //     if ((sl1 != null) && (sl2 != null)
  //         && ((sl1.shift == sl2.shift) && (sl1.shift != 0) || (sl2.shift != 0))) {
  //       // "size(a)-1 cmp size(b)-1"; should just use "size(a) cmp size(b)"
  //       return true;
  //     }
  //
  //     // This might never get invoked, as equality is printed out specially.
  //     VarInfo s1 = (sl1 == null) ? null : sl1.base;
  //     VarInfo s2 = (sl2 == null) ? null : sl2.base;
  //     if ((s1 != null) && (s2 != null)
  //         && (s1.equal_to == s2.equal_to)) {
  //       // lengths of equal arrays being compared
  //       return true;
  //     }
  //
  //     if (core.can_be_lt && (!core.can_be_eq)) {
  //       if ((sl2 != null) && (sl2.shift == 0)) {
  //         // "x < size(a)"  ("x <= size(a)-1" or "x < size(a)-1" would be more informative)
  //         return true;
  //       } else if ((sl1 != null) && (sl1.shift == -1)) {
  //         // "size(a)-1 < x"  ("size(a) <= x" would be more informative)
  //         return true;
  //       }
  //     } else if (core.can_be_gt && (!core.can_be_eq)) {
  //       if ((sl1 != null) && (sl1.shift == 0)) {
  //         // "size(a) > x"  ("size(a) >= x" would be more informative)
  //         return true;
  //       } else if ((sl2 != null) && (sl2.shift == -1)) {
  //         // "x > size(a)-1"  ("x >= size(a)" would be more informative)
  //         return true;
  //       }
  //     }
  //   }
  //
  //   return false;
  // }

}
