package daikon.inv.binary.sequenceScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import java.util.*;
import utilMDE.*;

// *****
// Automatically generated from Member-cpp.java
// *****

public final class Member extends SequenceScalar  {

  public final static boolean debugMember = false;
  // public final static boolean debugMember = true;

  protected Member(PptSlice ppt, boolean seq_first) {
    super(ppt, seq_first);
    Assert.assert(sclvar().rep_type == ProglangType.INT );
    Assert.assert(seqvar().rep_type == ProglangType.INT_ARRAY );
  }

  public static Member instantiate(PptSlice ppt, boolean seq_first) {
    VarInfo seqvar = ppt.var_infos[seq_first ? 0 : 1];
    VarInfo sclvar = ppt.var_infos[seq_first ? 1 : 0];

    if (isEqualToObviousMember(ppt, sclvar, seqvar)) {
      Global.implied_noninstantiated_invariants += 1;
      if (debugMember) {
        System.out.println("Member not instantiated (obvious): "
                           + sclvar.name + " in " + seqvar.name);
      }
      return null;
    }

    if (debugMember) {
      System.out.println("Member instantiated: "
                         + sclvar.name + " in " + seqvar.name);
    }
    return new Member(ppt, seq_first);
  }

  public boolean isObviousImplied() {
    VarInfo seqvar = ppt.var_infos[seq_first ? 0 : 1];
    VarInfo sclvar = ppt.var_infos[seq_first ? 1 : 0];
    return isEqualToObviousMember(ppt, sclvar, seqvar);
  }

  // Like isObviousMember, but also checks everything equal to the given
  // variables.
  public static boolean isEqualToObviousMember(PptSlice ppt, VarInfo sclvar, VarInfo seqvar) {
    Assert.assert(sclvar.isCanonical());
    Assert.assert(seqvar.isCanonical());
    Vector scl_equalto = sclvar.equalTo();
    scl_equalto.add(0, sclvar);
    Vector seq_equalto = seqvar.equalTo();
    seq_equalto.add(0, seqvar);

    for (int sclidx=0; sclidx<scl_equalto.size(); sclidx++) {
      for (int seqidx=0; seqidx<seq_equalto.size(); seqidx++) {
        VarInfo this_sclvar = (VarInfo) scl_equalto.elementAt(sclidx);
        VarInfo this_seqvar = (VarInfo) seq_equalto.elementAt(seqidx);
        if (isObviousMember(ppt, this_sclvar, this_seqvar))
          return true;
      }
    }
    return false;
  }

  public static boolean isObviousMember(PptSlice ppt, VarInfo sclvar, VarInfo seqvar) {

    VarInfo sclvar_seq = sclvar.isDerivedSequenceMember();
    // System.out.println("Member.isObviousMember being called");
    // System.out.println("sclvar=" + sclvar.name
    //                    + ", sclvar.derived=" + sclvar.derived
    //                    + ", sclvar_seq=" + sclvar_seq);

    if (sclvar_seq == null)
      return false;
    // The scalar is a member of the same array.
    if (sclvar_seq == seqvar)
      return true;
    // The scalar is a member of a different array than the sequence.
    // But maybe the relationship is still obvious, so keep checking.

    // If the scalar is a member of a subsequence of the sequence, then
    // the scalar is a member of the full sequence.
    // This is satisfied, for instance, when determining that
    // max(B[0..I]) is an obvious member of B.
    VarInfo sclseqsuper = sclvar_seq.isDerivedSubSequenceOf();
    if (sclseqsuper == seqvar)
      return true;

    // We know the scalar was derived from some array, but not from the
    // sequence variable.  If also not from what the sequence variable was
    // derived from, we don't know anything about membership.
    if (seqvar.isDerivedSubSequenceOf() != sclvar_seq)
      return false;

    // If the scalar is a positional element of the sequence from which
    // the sequence at hand was derived, then any relationship will be
    // (mostly) obvious by comparing the length of the sequence to the
    // index.  By contrast, if the scalar is max(...) or min(...), all bets
    // are off.

    // [Do I need to treat sclvar_seq.derived in {SequenceMin, SequenceMax}
    // specially?]

    // the sequence is B[0..J-1] or similar.  Get informatin about it
    SequenceScalarSubsequence  seqsss = (SequenceScalarSubsequence ) seqvar.derived;
    VarInfo seq_index = seqsss.sclvar();
    int seq_shift = seqsss.index_shift;
    boolean seq_from_start = seqsss.from_start;

    if (sclvar.derived instanceof SequenceScalarSubscript ) {
      // B[I] in B[0..J]
      // I need to compare I to J.  This is a stop-gap that does only
      // a bit of that.
      SequenceScalarSubscript  sclsss = (SequenceScalarSubscript ) sclvar.derived;
      VarInfo scl_index = sclsss.sclvar(); // "I" in "B[I]"
      int scl_shift = sclsss.index_shift;
      if (scl_index == seq_index) {
        // same variable: B[I] in B[0..I] or B[I] in B[I..]
        if (seq_from_start) {
          // same variable: B[I] in B[0..I]
          if (scl_shift <= seq_shift)
            return true;
        } else {
          // same variable: B[I] in B[I..]
          if (scl_shift >= seq_shift)
            return true;
        }
      } else {
        // different variables: B[I] in B[0..J] or B[I] in B[J..]
        PptSlice indices_ppt = ppt.parent.getView(scl_index, seq_index);
        if (indices_ppt != null) {
          boolean scl_is_var1 = (scl_index == indices_ppt.var_infos[0]);
          LinearBinary lb = LinearBinary.find(indices_ppt);
          long index_scl_minus_seq = -2222;          // valid only if lb != null
          if (lb != null) {
            if (!lb.justified()) {
              lb = null;
            } else if (lb.core.a != 1) {
              // Do not attempt to deal with anything but y=x+b.
              lb = null;
            } else {
              // lb.b is var2()-var1().
              index_scl_minus_seq = (scl_is_var1 ? -lb.core.b : lb.core.b);
              index_scl_minus_seq += scl_shift - seq_shift;
            }
          }
          // The LinearBinary gives more info than IntComparison would,
          // so only compute the IntComparison if no LinearBinary.
          IntComparison ic = (lb != null) ? null : IntComparison.find(indices_ppt);
          boolean scl_can_be_lt = false;		// valid only if ic != null
          boolean scl_can_be_eq = false;		// valid only if ic != null
          boolean scl_can_be_gt = false;		// valid only if ic != null
          if (ic != null) {
            if (! ic.justified()) {
              ic = null;
            } else {
              scl_can_be_eq = ic.core.can_be_eq;
              if (scl_is_var1) {
                scl_can_be_lt = ic.core.can_be_lt;
                scl_can_be_gt = ic.core.can_be_gt;
              } else {
                scl_can_be_lt = ic.core.can_be_gt;
                scl_can_be_gt = ic.core.can_be_lt;
              }
            }
          }

          if (seq_from_start) {
            // different variables: B[I] in B[0..J]
            if (lb != null) {
              if (index_scl_minus_seq <= 0)
                return true;
            } else if (ic != null) {
              // 4 cases:
              // (a) B[I] in B[0..J]
              // (b) B[I] in B[0..J-1]
              // (c) B[I-1] in B[0..J]
              // (d) B[I-1] in B[0..J-1]
              // 4 possible comparisons:
              // (e) I < J
              // (f) I <= J
              // (g) I >= J
              // (h) I > J
              // Combinations:  (filled in means true)
              //   abcd
              // e abcd
              // f a cd
              // g
              // h
              if ((scl_can_be_lt && ! scl_can_be_eq)
                  || (scl_can_be_lt && scl_can_be_eq && (scl_shift < seq_shift)))
                return true;
            }
          } else {
            // different variables: B[I] in B[J..]
            if (lb != null) {
              if (index_scl_minus_seq >= 0)
                return true;
            } else if (ic != null) {
              // 4 cases:
              // (a) B[I] in B[J..]
              // (b) B[I] in B[J+1..]
              // (c) B[I-1] in B[J..]
              // (d) B[I-1] in B[J+1..]
              // 4 possible comparisons:
              // (e) I < J
              // (f) I <= J
              // (g) I >= J
              // (h) I > J
              // Combinations:  (filled in means true)
              //   abcd
              // e
              // f
              // g a
              // h abc
              if ((scl_can_be_gt && (! scl_can_be_eq)
                   && (scl_shift + 1 >= seq_shift))
                  || (scl_can_be_gt && scl_can_be_eq
                      && (scl_shift == seq_shift)))
                return true;
            }
          }
        }
      } // end of different-variables case
    } else if (sclvar.derived instanceof SequenceInitial) {
      // System.out.println("sclvar derived from SequenceInitial: " + sclvar.name);

      // B[0] in B[0..J]; also B[-1] in B[J..]
      SequenceInitial sclse = (SequenceInitial) sclvar.derived;
      int scl_index = sclse.index;
      if (((scl_index == 0) && seq_from_start)
          || ((scl_index == -1) && !seq_from_start))
        // It might not be true, because the array could be empty;
        // but if the array isn't empty, then it's obvious.
        return true;
    }

    /// I need to test this code!
    // Now do tests over variable name, to avoid invariants like:
    //   header.next in header.~ll~next~
    //   header.next.element in header.~ll~next~.element
    //   header.next in header.next.~ll~next~
    //   return.current in return.current.~ll~next~
    String sclname = sclvar.name;
    String seqname = seqvar.name;
    int llpos = seqname.indexOf("~ll~");
    if (llpos != -1) {
      int tildepos = seqname.indexOf("~", llpos+5);
      if (tildepos != -1) {
        int midsize = tildepos-llpos-4;
        int lastsize = seqname.length()-tildepos-1;
        if (seqname.regionMatches(0, sclname, 0, llpos)
            && (((tildepos == seqname.length() - 1)
                 && (llpos == sclname.length()))
                || (seqname.regionMatches(llpos+4, sclname, llpos, midsize)
                    && seqname.regionMatches(tildepos+1, sclname, tildepos-4, lastsize))))
          return true;
      }
    }

    // int lastdot = sclvar.lastIndexOf(".");
    // if (lastdot != -1) {
    //   if (sclname.substring(0, lastdot).equals(seqname.substring(0, lastdot))
    //       && seqname.substring(lastdot).equals("~ll~" + sclname.substring(lastdot) + "~")) {
    //     return true;
    //   }
    // }

    return false;
  }

  public String repr() {
    double probability = getProbability();
    return "Member" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    return sclvar().name + " in " + seqvar().name;
  }

  public void add_modified(long [] a, long  i, int count) {
    if (ArraysMDE.indexOf(a, i) == -1) {
      if (debugMember) {
        System.out.println("Member destroyed:  " + format() + " because " + i + " not in " + ArraysMDE.toString(a));
      }
      destroy();
      return;
    }
  }

  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else
      return Invariant.PROBABILITY_JUSTIFIED;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof Member);
    return true;
  }
}
