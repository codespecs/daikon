package daikon.inv.sequenceScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public abstract class SequenceScalar extends Invariant {

  // By convention, the sequence is always passed in first
  public boolean seq_first;  // true if seq_index == 0 and scl_index == 1
  public int seq_index;                // 0 or 1
  public int scl_index;                // 0 or 1

  public SequenceScalar(PptSlice ppt_, boolean seq_first_) {
    super(ppt_);
    seq_first = seq_first_;
    if (seq_first) {
      seq_index = 0;
      scl_index = 1;
    } else {
      seq_index = 1;
      scl_index = 0;
    }
  }

  public VarInfo seqvar() {
    return ppt.var_infos[seq_index];
  }

  public VarInfo sclvar() {
    return ppt.var_infos[scl_index];
  }

  public void add(int[] v1, int mod1, int v2, int mod2, int count) {
    Assert.assert((mod1 == ValueTuple.MODIFIED)
		  || (mod1 == ValueTuple.UNMODIFIED));
    Assert.assert((mod2 == ValueTuple.MODIFIED)
		  || (mod2 == ValueTuple.UNMODIFIED));
    if ((mod1 == ValueTuple.MODIFIED)
	|| (mod2 == ValueTuple.MODIFIED))
      add_modified(v1, v2, count);
    else
      add_unmodified(v1, v2, count);
  }

  public abstract void add_modified(int[] v1, int v2, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can overrided this.
   */
  public void add_unmodified(int v1[], int v2, int count) {
    return;
  }


//   public SequenceScalar(PptSlice ppt_, VarInfo var_info1_, VarInfo var_info2_) {
//     super(ppt_);
//     var_info1 = var_info1_;
//     var_info2 = var_info2_;
//   }
//
//   public boolean usesVar(VarInfo vi) {
//     return (var_info1 == vi) || (var_info2 == vi);
//   }

}
