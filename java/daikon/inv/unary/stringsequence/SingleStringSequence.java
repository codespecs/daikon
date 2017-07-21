package daikon.inv.unary.stringsequence;

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

/** Abstract base class for invariants over one variable of type {@code String[]}. */
public abstract class SingleStringSequence extends UnaryInvariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected SingleStringSequence(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ SingleStringSequence() {
    super();
  }

  /** Returns whether or not the specified types are valid. */
  @Override
  public final boolean valid_types(VarInfo[] vis) {
    return ((vis.length == 1)
        && vis[0].file_rep_type.baseIsString()
        && vis[0].file_rep_type.isArray());
  }

  public VarInfo var(
      /*>>>@GuardSatisfied @UnknownInitialization(SingleStringSequence.class) @Raw(SingleStringSequence.class) SingleStringSequence this*/) {
    return ppt.var_infos[0];
  }

  // Should never be called with modified == ValueTuple.MISSING_NONSENSICAL.
  // Subclasses need not override this except in special cases;
  // just implement @link{add_modified(Object,int)}.
  @Override
  public InvariantStatus add(/*@Interned*/ Object val, int mod_index, int count) {
    assert !falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    assert Intern.isInterned(val);
    // System.out.println("SingleStringSequence.add(" + ArraysMDE.toString(value) + ", " + modified + ", " + count + ")");
    /*@Interned*/ String[] value = (/*@Interned*/ String[]) val;
    if (value == null) {
    } else if (mod_index == 0) {
      return add_unmodified(value, count);
    } else {
      return add_modified(value, count);
    }
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus check(/*@Interned*/ Object val, int mod_index, int count) {
    assert !falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    assert Intern.isInterned(val);
    /*@Interned*/ String[] value = (/*@Interned*/ String[]) val;
    if (value == null) {
      return InvariantStatus.NO_CHANGE;
    } else if (mod_index == 0) {
      return check_unmodified(value, count);
    } else {
      return check_modified(value, count);
    }
  }

  /**
   * Presents a sample to the invariant. Returns whether the sample is consistent with the
   * invariant. Does not change the state of the invariant.
   *
   * @param count how many identical samples were observed in a row. For example, three calls to
   *     check_modified with a count parameter of 1 is equivalent to one call to check_modified with
   *     a count parameter of 3.
   * @return whether or not the sample is consistent with the invariant
   */
  public abstract InvariantStatus check_modified(
      /*@Interned*/ String /*@Interned*/ [] value, int count);

  public InvariantStatus check_unmodified(/*@Interned*/ String /*@Interned*/ [] value, int count) {
    return InvariantStatus.NO_CHANGE;
  }

  /**
   * Similar to {@link #check_modified} except that it can change the state of the invariant if
   * necessary. If the invariant doesn't have any state, then the implementation should simply call
   * {@link #check_modified}. This method need not check for falsification; that is done by the
   * caller.
   */
  public abstract InvariantStatus add_modified(
      /*@Interned*/ String /*@Interned*/ [] value, int count);

  /** By default, do nothing if the value hasn't been seen yet. Subclasses can override this. */
  public InvariantStatus add_unmodified(/*@Interned*/ String /*@Interned*/ [] value, int count) {
    return InvariantStatus.NO_CHANGE;
  }
}
