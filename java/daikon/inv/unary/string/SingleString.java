package daikon.inv.unary.string;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

public abstract class SingleString extends Invariant {

  protected SingleString(PptSlice ppt) {
    super(ppt);
    // System.out.println("Created SingleString invariant " + this + " at " + ppt);
  }

  public VarInfo var() {
    return ppt.var_infos[0];
  }

  // Should never be called with modified == ValueTuple.MISSING.
  // Subclasses need not override this except in special cases;
  // just implement @link{add_modified(String,int)}.
  public void add(String value, int mod_index, int count) {
    Assert.assert(! no_invariant);
    Assert.assert((mod_index >= 0) && (mod_index < 2));
    Assert.assert(!finished);
    if (mod_index == 0) {
      add_unmodified(value, count);
    } else {
      add_modified(value, count);
    }
  }

  /**
   * This method need not check for no_invariant;
   * that is done by the caller.
   **/
  public abstract void add_modified(String value, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can override this.
   **/
  public void add_unmodified(String value, int count) {
    return;
  }

}
