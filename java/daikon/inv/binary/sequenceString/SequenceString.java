package daikon.inv.binary.sequenceString;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;

import utilMDE.*;

public abstract class SequenceString
  extends BinaryInvariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // By convention, the sequence is always passed in first
  public boolean seq_first;  // true if seq_index == 0 and scl_index == 1
  public int seq_index;                // 0 or 1
  public int scl_index;                // 0 or 1

  protected SequenceString(PptSlice ppt, boolean seq_first) {
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

  protected Invariant resurrect_done_swapped() {
    seq_first = !seq_first;
    seq_index = seq_first ? 0 : 1;
    scl_index = seq_first ? 1 : 0;
    return this;
  }

  public VarInfo seqvar() {
    return ppt.var_infos[seq_index];
  }

  public VarInfo sclvar() {
    return ppt.var_infos[scl_index];
  }

  public void add(String[] v1, String v2, int mod_index, int count) {
    Assert.assert(! no_invariant);
    Assert.assert((mod_index >= 0) && (mod_index < 4));
    if (v1 == null) {
      // ppt.var_infos[seq_index].canBeNull = true; // [[INCR]]
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
  public abstract void add_modified(String[] v1, String v2, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can override this.
   **/
  public void add_unmodified(String[] v1, String v2, int count) {
    return;
  }

}
