package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public abstract class ThreeScalar
  extends Invariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected ThreeScalar(PptSlice ppt) {
    super(ppt);
  }

  public VarInfo var1() {
    return ppt.var_infos[0];
  }

  public VarInfo var2() {
    return ppt.var_infos[1];
  }

  public VarInfo var3() {
    return ppt.var_infos[2];
  }

  public void add(long v1, long v2, long v3, int mod_index, int count) {
    // Tests for whether a value is missing should be performed before
    // making this call, so as to reduce overall work.
    Assert.assert(! no_invariant);
    Assert.assert((mod_index >= 0) && (mod_index < 8));
    if (mod_index == 0) {
      add_unmodified(v1, v2, v3, count);
    } else {
      add_modified(v1, v2, v3, count);
    }
  }

  /**
   * This method need not check for no_invariant;
   * that is done by the caller.
   **/
  public abstract void add_modified(long v1, long v2, long v3, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can override this.
   **/
  public void add_unmodified(long v1, long v2, long v3, int count) {
    return;
  }

}
