package daikon.inv.unary.string;

import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.UnaryInvariant;
import plume.*;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import typequals.*;
*/

/**
 * Abstract base class used to evaluate single strings.
 **/

public abstract class SingleString
  extends UnaryInvariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected SingleString(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ SingleString() {
    super();
  }

  /** Returns whether or not the specified types are valid for unary string **/
  public final boolean valid_types (VarInfo[] vis) {
    return ((vis.length == 1) && vis[0].file_rep_type.isString());
  }

  public VarInfo var(/*>>>@GuardSatisfied @UnknownInitialization(SingleString.class) @Raw(SingleString.class) SingleString this*/) {
    return ppt.var_infos[0];
  }

  // Should never be called with modified == ValueTuple.MISSING_NONSENSICAL.
  // Subclasses need not override this except in special cases;
  // just implement @link{add_modified(String,int)}.
  public InvariantStatus add(/*@Interned*/ Object val, int mod_index, int count) {
    assert ! falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    String value = (String) val;
    if (mod_index == 0) {
      return add_unmodified(value, count);
    } else {
      return add_modified(value, count);
    }
  }

  public InvariantStatus check(/*@Interned*/ Object val, int mod_index, int count) {
    assert ! falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    String value = (String) val;
    if (mod_index == 0) {
      return check_unmodified(value, count);
    } else {
      return check_modified(value, count);
    }
  }


  /**
   * This method need not check for falsified;
   * that is done by the caller.
   **/
  public abstract InvariantStatus add_modified(/*@Interned*/ String value, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can override this.
   **/
  public InvariantStatus add_unmodified(/*@Interned*/ String value, int count) {
    return InvariantStatus.NO_CHANGE;
  }

  public abstract InvariantStatus check_modified(/*@Interned*/ String value, int count);

  public InvariantStatus check_unmodified(/*@Interned*/ String value, int count) {
    return InvariantStatus.NO_CHANGE;
  }



}
