package daikon.inv.unary.string;

import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.*;
import utilMDE.*;

public abstract class SingleString
  extends UnaryInvariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

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
    Assert.assert(! falsified);
    Assert.assert((mod_index >= 0) && (mod_index < 2));
    if (mod_index == 0) {
      add_unmodified(value, count);
    } else {
      add_modified(value, count);
    }
  }

  /**
   * This method need not check for falsified;
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
