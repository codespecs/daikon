package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;

import java.util.*;
import java.io.Serializable;

import org.apache.log4j.Category;

import utilMDE.Assert;
import utilMDE.MathMDE;

/**
 * Generates suppressions for variables.  That is, this scans an
 * invariant to see if it contains variables that are not interesting,
 * and suppresses the invariants.  A variable is not interesting if
 * some other invariant says something "obvious" about its value.
 **/

public abstract class VarSuppressionFactory {

  /**
   * General debug tracer.
   **/
  public static final Category debug =
    Category.getInstance ("daikon.suppress.VarSuppresionFactory");

  /**
   * Check variables of a given invariant.  If any of them should be
   * suppressed, then this invariant should be too.
   * @return can be null
   **/

  public SuppressionLink generateSuppressionLink (Invariant inv) {
    for (int i = 0; i < inv.ppt.var_infos.length; i++) {
      SuppressionLink link = generateSuppressionLink (inv.ppt.var_infos[i]);
      if (link != null) {
        if (debug.isDebugEnabled()) {
          debug.debug ("The invariant " + inv.repr() + " is suppressed by " + link);
        }
	return link;
      }
    }
    return null;
  }


  /**
   * Whether a variable should be suppressed.
   * @return a SuppressionLink of the invariants that suppress vi, or
   * null if vi isn't suppressed.
   **/
  protected abstract SuppressionLink generateSuppressionLink (VarInfo vi);

}
