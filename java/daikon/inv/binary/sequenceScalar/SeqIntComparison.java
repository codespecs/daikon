package daikon.inv.binary.sequenceScalar;

import daikon.*;
import daikon.derive.unary.*;
import daikon.inv.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoSequence.*;
import java.util.*;
import utilMDE.*;

public final class SeqIntComparison extends SequenceScalar {

  public IntComparisonCore core;

  static boolean debugSeqIntComparison = false;

  protected SeqIntComparison(PptSlice ppt, boolean seq_first, boolean only_eq, boolean obvious_le, boolean obvious_ge) {
    super(ppt, seq_first);
    Assert.assert(sclvar().rep_type == ProglangType.INT);
    Assert.assert(seqvar().rep_type == ProglangType.INT_ARRAY);
    core = new IntComparisonCore(this, only_eq, false, false, obvious_le, obvious_ge);
  }

  public static SeqIntComparison instantiate(PptSlice ppt, boolean seq_first) {
    VarInfo seqvar = ppt.var_infos[seq_first ? 0 : 1];
    VarInfo sclvar = ppt.var_infos[seq_first ? 1 : 0];

    // if (isEqualToObviousSeqIntComparison(sclvar, seqvar)) {
    //   Global.implied_noninstantiated_invariants += 1;
    //   if (debugSeqIntComparison) {
    //     System.out.println("SeqIntComparison not instantiated (obvious): "
    //                        + sclvar.name + " vs. " + seqvar.name);
    //   }
    //   return null;
    // }

    if (debugSeqIntComparison) {
      System.out.println("SeqIntComparison instantiated: "
                         + sclvar.name + " vs. " + seqvar.name);
    }

    SequenceMin seqmin = null;
    SequenceMax seqmax = null;
    VarInfo sclseq = null;
    if (sclvar.derived instanceof SequenceMin) {
      seqmin = (SequenceMin) sclvar.derived;
      sclseq = seqmin.base;
    } else if (sclvar.derived instanceof SequenceMax) {
      seqmax = (SequenceMax) sclvar.derived;
      sclseq = seqmax.base;
    }
    if (seqvar == sclseq) {
      return null;
    }
    boolean obvious_lt = false;
    boolean obvious_le = false;
    boolean obvious_gt = false;
    boolean obvious_ge = false;
    if ((sclseq != null) && (SubSequence.isObviousDerived(seqvar, sclseq))) {
      // System.out.println("SeqIntComparison instantiate; is obvious derived: " + seqvar.name + " " + sclvar.name + " " + sclseq.name);
      obvious_ge = (seqmin != null);
      obvious_le = (seqmax != null);
    }
    // if (sclseq != null) {
    //   System.out.println("SeqIntComparison instantiate: " + seqvar.name + " " + sclvar.name + " " + sclseq.name + " obvious_le=" + obvious_le + ", obvious_ge=" + obvious_ge);
    // }

    // Don't compute ordering relationships over object addresses for
    // elements of a Vector.  (But do compute equality/constant!)
    ProglangType elt_type = seqvar.type.elementType();
    // Assert.assert(elt_type == sclvar.type, "Different types: " + elt_type.format() + " " + seqvar.name + " elements; " + sclvar.type.format() + " " + sclvar.name);
    Assert.assert(elt_type.comparable(sclvar.type));
    boolean only_eq = ! seqvar.type.elementIsIntegral();
    return new SeqIntComparison(ppt, seq_first, only_eq, obvious_le, obvious_ge);
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
    String comparator = core.format_comparator();
    return seqvar().name.name() + " elements " + comparator + " " + sclvar().name.name();
  }

  public String format_esc() {
    String comparator = core.format_comparator();
    String[] form =
      VarInfoName.QuantHelper.format_esc(new VarInfoName[]
	{ seqvar().name, sclvar().name });
    return form[0] + "(" + form[1] + " " + comparator + " " + form[2] + ")" + form[3];
  }

  public String format_simplify() {
    return "format_simplify " + this.getClass() + " needs to be changed: " + format();
  }

  public void add_modified(long [] a, long x, int count) {
    for (int i=0; i<a.length; i++) {
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
  public boolean isObviousImplied() {
    if (isExact()) {
      return false;
    }

    VarInfo seqvar = seqvar();
    VarInfo sclvar = sclvar();
    if (sclvar.isDerived() && (sclvar.derived instanceof SequenceLength)) {
      // Sequence length tests
      SequenceLength scl_seqlen = (SequenceLength) sclvar.derived;

      if (core.can_be_lt && (!core.can_be_eq)) {
        if ((scl_seqlen != null) && (scl_seqlen.shift == 0)) {
          // "x < size(a)"  ("x <= size(a)-1" would be more informative)
          return true;
        }
      } else if (core.can_be_gt && (!core.can_be_eq)) {
        if ((scl_seqlen != null) && (scl_seqlen.shift == -1)) {
          // "x > size(a)-1"  ("x >= size(a)" would be more informative)
          return true;
        }
      }
    }

    PptTopLevel pptt = (PptTopLevel) ppt.parent;

    {
      PptSlice1 seqslice = pptt.findSlice(seqvar);
      EltOneOf eoo = EltOneOf.find(seqslice);
      if ((eoo != null) && eoo.enoughSamples() && (eoo.num_elts() == 1)) {
        return true;
      }
    }


    // For each other sequence variable, if it is a supersequence of this
    // one and it has the same invariant, then this one is obvious.
    for (int i=0; i<pptt.var_infos.length; i++) {
      VarInfo vi = pptt.var_infos[i];
      if (SubSequence.isObviousDerived(seqvar, vi)) {
        PptSlice2 other_slice = pptt.findSlice_unordered(vi, sclvar());
        if (other_slice != null) {
          SeqIntComparison other_sic = SeqIntComparison.find(other_slice);
          if ((other_sic != null) && other_sic.enoughSamples()) {
            return true;
          }
        }
      }
    }


    // {
    //   if (sclvar.isDerived() && ((sclvar.derived instanceof SequenceMin)
    //                              || (sclvar.derived instanceof SequenceMax))) {
    //     SequenceMin seqmin = null;
    //     SequenceMin seqmax = null;
    //     VarInfo sclseq;
    //     if (sclvar.derived instanceof SequenceMin) {
    //       seqmin = (SequenceMin sclvar.derived);
    //       sclseq = seqmin.base;
    //     } else if (sclvar.derived instanceof SequenceMax) {
    //       seqmax = (SequenceMax sclvar.derived);
    //       sclseq = seqmax.base;
    //     } else {
    //       throw new Error("Can't happen");
    //     }
    //     if (seqvar.equal_to == sclseq.equal_to) {
    //       return true;
    //     }
    //     if (seqvar.isEqualToObviousMember
    //
    //     // Sequence max/min tests
    //   // This might never get invoked, as equality is printed out specially.
    //   SequenceLength scl_seqlen = (SequenceLength) sclvar.derived;
    //
    //   VarInfo sclseq = (scl_seqlen == null) ? null : scl_seqlen.base;
    //
    //
    //   if ((sclseq != null)
    //       && (seqvar.equal_to == sclseq.equal_to)) {
    //     // lengths of equal arrays being compared
    //     return true;
    //   }






    return false;
  }

}
