package daikon.inv.sequenceScalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;
import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;

// Similar to NonAliased; if I change this, consider changing it, too.
public class Member extends SequenceScalar {

  protected Member(PptSlice ppt_, boolean seq_first_) {
    super(ppt_, seq_first_);
    Assert.assert(sclvar().rep_type == ProglangType.INT);
    Assert.assert(seqvar().rep_type == ProglangType.INT_ARRAY);
  }

  public static Member instantiate(PptSlice ppt, boolean seq_first) {
    VarInfo seqvar = ppt.var_infos[seq_first ? 0 : 1];
    VarInfo sclvar = ppt.var_infos[seq_first ? 1 : 0];

    VarInfo sclvar_seq = sclvar.isDerivedSequenceMember();
    // System.out.println("Member.instantiate being called");
    // System.out.println("sclvar=" + sclvar.name
    //                    + ", sclvar.derived=" + sclvar.derived
    //                    + ", sclvar_seq=" + sclvar_seq);

    if (sclvar_seq != null) {

      if (sclvar_seq == seqvar) {
        return null;
      }

      // The scalar is a member of a different array than the sequence.
      // But maybe the relationshiop is still obvious.

      Derivation sclvar_seq_deriv = sclvar_seq.derived;
      // If the scalar is an element of a subsequence of the sequence,
      // the relationship is obvious.
      if ((sclvar_seq_deriv != null)
          && (sclvar_seq_deriv instanceof SequenceScalarSubsequence)
          && ((SequenceScalarSubsequence)sclvar_seq_deriv).seqvar() == seqvar) {
        return null;
      }

      // If the scalar is a positional element of the sequence from which
      // the sequence at hand was derived, then any relationship will be
      // (mostly) obvious by comparing the length of the sequence to the
      // index.  I could refine this by looking at those relationships and
      // not forbidding A[i] to be compared to A[0..j] if i is sometimes
      // greater than j.
      Derivation sclvar_deriv = sclvar_seq.derived;
      if (! ((sclvar_deriv instanceof SequenceMax)
             || (sclvar_deriv instanceof SequenceMin))) {
        // sclvar_deriv is a positional element
        Derivation seqvar_deriv = seqvar.derived;
        if (seqvar_deriv != null) {
          if (seqvar_deriv instanceof SequenceScalarSubsequence) {
            SequenceScalarSubsequence sss = (SequenceScalarSubsequence) seqvar_deriv;
            if (sclvar_seq == sss.seqvar())
              return null;
          }
        }
      }
    }

    return new Member(ppt, seq_first);
  }

  public static boolean isObviousMember(VarInfo sclvar, VarInfo seqvar) {

    VarInfo sclseq = sclvar.isDerivedSequenceMember();
    if (sclseq == null)
      return false;
    if (sclseq == seqvar)
      return true;
    // This is satisfied, for instance, when determining that
    // max(B[0..I]) is an obvious member of B.
    VarInfo sclseqsuper = sclseq.isDerivedSubSequenceOf();
    if (sclseqsuper == seqvar)
      return true;

    if (seqvar.isDerivedSubSequenceOf() == sclseq) {
      // B[I] in B[0..J]
      // I need to compare I to J.  This is a stop-gap that does only
      // a bit of that.
      // Need to completely cover the cases of
      // SequenceExtremum and SequenceScalarSubscript.

      SequenceScalarSubsequence seqsss = (SequenceScalarSubsequence) seqvar.derived;
      VarInfo seq_index = seqsss.sclvar();
      int seq_shift = seqsss.index_shift;

      if (sclvar.derived instanceof SequenceScalarSubscript) {
        SequenceScalarSubscript sclsss = (SequenceScalarSubscript) sclvar.derived;
        VarInfo scl_index = sclsss.sclvar();
        int scl_shift = sclsss.index_shift;
        if ((scl_index == seq_index)
            && (! ((scl_shift == -1) && (seq_shift == 0))))
          return true;
      } else if (sclvar.derived instanceof SequenceExtremum) {
        SequenceExtremum sclse = (SequenceExtremum) sclvar.derived;
        int scl_index = sclse.index;
        if (scl_index == 0)
          // It might not be true, because the array could be empty;
          // but if the array isn't empty, then it's obvious.
          return true;
      }
    }
    return false;
  }


  public String repr() {
    double probability = getProbability();
    return "Member" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    if ((!no_invariant) && justified()) {
      return sclvar().name + " in " + seqvar().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a, int i, int count) {
    if (ArraysMDE.indexOf(a, i) == -1) {
      destroy();
      return;
    }
  }


  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }

}

