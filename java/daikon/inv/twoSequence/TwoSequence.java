package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public abstract class TwoSequence extends Invariant {

  protected TwoSequence(PptSlice ppt_) {
    super(ppt_);
  }

  public VarInfo var1() {
    return ppt.var_infos[0];
  }

  public VarInfo var2() {
    return ppt.var_infos[1];
  }

  public void add(int[] v1, int mod1, int[] v2, int mod2, int count) {
    Assert.assert(! no_invariant);
    Assert.assert((mod1 == ValueTuple.MODIFIED)
		  || (mod1 == ValueTuple.UNMODIFIED));
    Assert.assert((mod2 == ValueTuple.MODIFIED)
		  || (mod2 == ValueTuple.UNMODIFIED));
    if (finished)
      return;
    if ((mod1 == ValueTuple.MODIFIED)
	|| (mod2 == ValueTuple.MODIFIED)) {
      add_modified(v1, v2, count);
    } else {
      add_unmodified(v1, v2, count);
    }
  }

  public abstract void add_modified(int[] v1, int[] v2, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can overrided this.
   */
  public void add_unmodified(int[] v1, int[] v2, int count) {
    return;
  }

}
