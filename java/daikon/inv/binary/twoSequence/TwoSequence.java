package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public abstract class TwoSequence extends Invariant {

  protected TwoSequence(PptSlice ppt) {
    super(ppt);
  }

  public VarInfo var1() {
    return ppt.var_infos[0];
  }

  public VarInfo var2() {
    return ppt.var_infos[1];
  }

  public void add(long[] v1, long[] v2, int mod_index, int count) {
    Assert.assert(! no_invariant);
    Assert.assert((mod_index >= 0) && (mod_index < 4));
    Assert.assert(!finished);
    if (v1 == null) {
      // ppt.var_infos[0].canBeNull = true; // [[INCR]]
    } else if (v2 == null) {
      // ppt.var_infos[1].canBeNull = true; // [[INCR]]
    } else if (mod_index == 0) {
      add_unmodified(v1, v2, count);
    } else {
      add_modified(v1, v2, count);
    }
  }

  public abstract void add_modified(long[] v1, long[] v2, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can override this.
   **/
  public void add_unmodified(long[] v1, long[] v2, int count) {
    return;
  }

}
