package daikon.inv.binary.sequenceScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public abstract class SequenceScalar extends Invariant {

  // By convention, the sequence is always passed in first
  public boolean seq_first;  // true if seq_index == 0 and scl_index == 1
  public final int seq_index;                // 0 or 1
  public final int scl_index;                // 0 or 1

  protected SequenceScalar(PptSlice ppt, boolean seq_first) {
    super(ppt);
    this.seq_first = seq_first;
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

  public void add(long[] v1, long v2, int mod_index, int count) {
    Assert.assert(! no_invariant);
    Assert.assert((mod_index >= 0) && (mod_index < 4));
    Assert.assert(!finished);
    if (v1 == null) {
      ppt.var_infos[seq_index].canBeNull = true;
    } else if (mod_index == 0) {
      add_unmodified(v1, v2, count);
    } else {
      add_modified(v1, v2, count);
    }
  }

  /**
   * This method need not check for no_invariant;
   * that is done by the caller.
   **/
  public abstract void add_modified(long[] v1, long v2, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can override this.
   **/
  public void add_unmodified(long[] v1, long v2, int count) {
    return;
  }

}
