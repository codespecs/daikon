package daikon.inv.sequenceScalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;

// Similar to NonAliased; if I change this, consider changing it, too.
public class Member extends SequenceScalar {

  boolean nonmember = false;

  Member(PptSlice ppt_, boolean seq_first_) {
    super(ppt_, seq_first_);
  }

  public String repr() {
    double probability = getProbability();
    return "Member" + varNames() + ": "
      + "nonmember=" + nonmember
      + "; probability = " + probability;
  }

  public String format() {
    if ((!nonmember) && justified()) {
      return sclvar().name + " in " + seqvar().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] a, int i, int count) {
    if (nonmember)
      return;
    nonmember = (ArraysMDE.indexOf(a, i) == -1);
  }


  protected double computeProbability() {
    if (nonmember)
      return Invariant.PROBABILITY_NEVER;
    else
      return 0;
  }

  public boolean isObvious() {
    VarInfo sclvar = sclvar();
    // System.out.println("Member.isObvious being called for " + repr());
    // System.out.println("sclvar.derived=" + sclvar.derived);
    if (sclvar.derived != null) {
      if (sclvar.derived instanceof SequenceScalarSubscript) {
        SequenceScalarSubscript sss = (SequenceScalarSubscript) sclvar.derived;
        // System.out.println("sss.seqvar()=" + sss.seqvar() + "seqvar()=" + seqvar());
        if (sss.seqvar() == seqvar())
          return true;
      } else if (sclvar.derived instanceof SequenceExtremum) {
        SequenceExtremum se = (SequenceExtremum) sclvar.derived;
        // System.out.println("se.seqvar()=" + se.seqvar() + "seqvar()=" + seqvar());
        if (se.seqvar() == seqvar())
          return true;
      }
    }
    return false;
  }

}

//         # For each (num, sequence), determine if num is a member of seq
//         self.member_obvious = ((seqvar + "[" == sclvar[0:len(seqvar)+1])
//                                or ("min(" + seqvar + ")" == sclvar)
//                                or ("max(" + seqvar + ")" == sclvar))
