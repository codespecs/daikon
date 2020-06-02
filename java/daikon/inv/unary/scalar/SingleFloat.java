package daikon.inv.unary.scalar;

import daikon.PptSlice;
import daikon.VarInfo;
import daikon.inv.InvariantStatus;
import daikon.inv.unary.UnaryInvariant;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import typequals.prototype.qual.Prototype;

/** Abstract base class for invariants over one variable of type {@code double}. */
public abstract class SingleFloat extends UnaryInvariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected SingleFloat(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SingleFloat() {
    super();
  }

  /** Returns whether or not the specified types are valid for unary float. */
  @Override
  public final boolean valid_types(VarInfo[] vis) {
    return ((vis.length == 1) && vis[0].file_rep_type.isFloat());
  }

  public VarInfo var(@GuardSatisfied @UnknownInitialization(SingleFloat.class) SingleFloat this) {
    return ppt.var_infos[0];
  }

  // Should never be called with modified == ValueTuple.MISSING_NONSENSICAL.
  // Subclasses need not override this except in special cases;
  // just implement {@link add_modified(Object,int)}.
  @Override
  public InvariantStatus add(@Interned Object val, int mod_index, int count) {
    assert !falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    double value = ((Double) val).doubleValue();
    if (mod_index == 0) {
      return add_unmodified(value, count);
    } else {
      return add_modified(value, count);
    }
  }

  @Override
  public InvariantStatus check(@Interned Object val, int mod_index, int count) {
    assert !falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    double value = ((Double) val).doubleValue();
    if (mod_index == 0) {
      return check_unmodified(value, count);
    } else {
      return check_modified(value, count);
    }
  }

  /**
   * Similar to {@link #check_modified} except that it can change the state of the invariant if
   * necessary. If the invariant doesn't have any state, then the implementation should simply call
   * {@link #check_modified}. This method need not check for falsification; that is done by the
   * caller.
   */
  public abstract InvariantStatus add_modified(double value, int count);

  /** By default, do nothing if the value hasn't been seen yet. Subclasses can override this. */
  public InvariantStatus add_unmodified(double value, int count) {
    // System.out.println("SingleFloat.add_unmodified " + ppt.name() + ": parent=" + ppt.parent);
    return InvariantStatus.NO_CHANGE;
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
  public abstract InvariantStatus check_modified(double value, int count);

  public InvariantStatus check_unmodified(double value, int count) {
    return InvariantStatus.NO_CHANGE;
  }

  // This has no additional suppression factories beyond those of Invariant.
}
