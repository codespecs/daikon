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
    Assert.assert(! sclvar().type.isArray());
    Assert.assert(seqvar().type.isArray());
  }

  public static Member instantiate(PptSlice ppt, boolean seq_first) {
    VarInfo seqvar = ppt.var_infos[seq_first ? 0 : 1];
    VarInfo sclvar = ppt.var_infos[seq_first ? 1 : 0];

    VarInfo sclvar_seq = sclvar.isObviousSequenceMember();
    // System.out.println("Member.instantiate being called");
    // System.out.println("sclvar=" + sclvar.name
    //                    + ", sclvar.derived=" + sclvar.derived
    //                    + ", sclvar_seq=" + sclvar_seq);

    if (sclvar_seq == seqvar) {
      return null;
    }
    // If the scalar is a positional element of the sequence from which the
    // sequence at hand was derived, then any relationship will be (mostly)
    // obvious by comparing the length of the sequence to the index.  I
    // could refine this by looking at those relationships and not
    // forbidding A[i] to be compared to A[0..j] if i is sometimes greater
    // than j.
    if (sclvar_seq != null) {
      Derivation sclvar_deriv = sclvar_seq.derived;
      if (! ((sclvar_deriv instanceof SequenceMax)
             || (sclvar_deriv instanceof SequenceMin))) {
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
    no_invariant = (ArraysMDE.indexOf(a, i) == -1);
  }


  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }

}

