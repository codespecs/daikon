package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.Logger;

/**
 * Defines a suppressee for non-instantiating suppression.  A suppressee
 * consists only of the class at this point since ternary invariants
 * only require the class to define them fully (permutations are built
 * into the class name).  When binary invariants are suppressed additional
 * information will need to be included.
 */
public class NISuppressee {

  Class sup_class;
  int var_count;
  Invariant sample_inv;

  public NISuppressee (Class cls, int var_count) {
    sup_class = cls;
    Assert.assertTrue ((var_count >= 1) && (var_count <=3));
    this.var_count = var_count;

    try {
      Method instantiate = cls.getMethod ("instantiate",
                                                new Class[] {PptSlice.class});
      sample_inv = (Invariant)instantiate.invoke (null, new Object[] {null});
    } catch (Exception e) {
      throw new RuntimeException ("error instantiating invariant "
                                  + cls.getName() + ": " + e);
    }
  }

  /**
   * Instantiates the suppressee invariant on the specified slice.
   */
  public Invariant instantiate (PptSlice slice) {

    Invariant inv = (Invariant) sample_inv.clone();
    inv.ppt = slice;
    return (inv);
  }

  public String toString() {

    return (UtilMDE.unqualified_name (sup_class));
  }

}
