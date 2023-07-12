package daikon.inv;

import daikon.PptSlice;
import daikon.PptTopLevel;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;

/**
 * This is a special implication invariant that guards any invariants that are over variables that
 * are sometimes missing. For example, if the invariant {@code a.x = 0} is true, the guarded
 * implication is {@code a != null => a.x = 0}.
 */
public class GuardingImplication extends Implication {
  static final long serialVersionUID = 20020725L;

  private GuardingImplication(
      PptSlice ppt, Invariant predicate, Invariant consequent, boolean iff) {
    super(ppt, predicate, consequent, iff, predicate, consequent);
  }

  // Do not call this!  The only location these should be created is in
  // Invariant.createGuardedInvariant().  (I need to find a way to enforce this.)
  // A GuardingImplication is never installed in a PptMap -- it's only
  // printed by using format_using.
  public static GuardingImplication makeGuardingImplication(
      PptTopLevel ppt, Invariant predicate, Invariant consequent, boolean iff) {
    assert predicate != null;
    assert consequent != null;

    // No duplicate check because the way it is set up no duplicates should
    // occur:  No invariants are duplicates, and since each guarding
    // implication is based off of an existing invariant in a PptSlice, we
    // are guarenteed no duplicate guarding implications exist.

    GuardingImplication result =
        new GuardingImplication(ppt.joiner_view, predicate, consequent, iff);
    return result;
  }

  @Pure
  @Override
  public boolean isWorthPrinting() {
    return right.isWorthPrinting();
    // return !right.isObvious();
  }

  @Override
  public boolean enoughSamples(@GuardSatisfied GuardingImplication this) {
    return right.enoughSamples();
  }

  @Override
  public double computeConfidence() {
    return right.computeConfidence();
  }
}
