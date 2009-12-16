package daikon.inv;

import daikon.*;

/**
 *  This is a special implication invariant that guards any invariants that
 *  are over variables that are sometimes missing.  For example, if the
 *  invariant <samp>a.x = 0</samp> is true, the guarded implication is
 *  <samp>a != null => a.x = 0</samp>.
 **/
public class GuardingImplication
  extends Implication
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020725L;

  private GuardingImplication(PptSlice ppt, Invariant predicate, Invariant consequent, boolean iff) {
    super(ppt, predicate, consequent, iff, null, null);
  }

  // The below is not true any longer.  PptSlice.guardInvariants no longer
  // exists, and this method is called only in createGuardedInvariant().
  // // Do not call this!  The only location these should be created is in
  // // PptSlice.guardInvariants().  (I need to find a way to enforce this.)
  public static GuardingImplication makeGuardingImplication(PptTopLevel ppt,
                                                            Invariant predicate,
                                                            Invariant consequent,
                                                            boolean iff) {
    assert predicate != null;
    assert consequent != null;

    // No duplicate check because the way it is set up no duplicates should
    // occur:  No invariants are duplicates, and since each guarding
    // implication is based off of an existing invariant in a PptSlice, we
    // are guarenteed no duplicate guarding implications exist.

    GuardingImplication result = new GuardingImplication(ppt.joiner_view, predicate, consequent, iff);
    return result;
  }

  public boolean isWorthPrinting() {
    return right.isWorthPrinting();
    // return !right.isObvious();
  }

  public boolean enoughSamples() {
    return right.enoughSamples();
  }

  public double computeConfidence() {
    return right.computeConfidence();
  }

}
