package daikon.inv.sequenceScalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;


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
}

//         # For each (num, sequence), determine if num is a member of seq
//         self.member_obvious = ((seqvar + "[" == sclvar[0:len(seqvar)+1])
//                                or ("min(" + seqvar + ")" == sclvar)
//                                or ("max(" + seqvar + ")" == sclvar))
