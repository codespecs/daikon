package daikon.inv;

import daikon.*;

import java.util.*;

public class GuardingImplication
  extends Implication
{
  static final long serialVersionUID = 20020725L;

  private GuardingImplication(PptSlice ppt, Invariant predicate, Invariant consequent, boolean iff) {
    super(ppt, predicate, consequent, iff);
  }

  // Trying to figure out a better way to enforce this, but for now, do not
  // create these... only location these should be created is in the PptSlice
  // function guardInvariants
  public static GuardingImplication makeGuardingImplication(PptTopLevel ppt,
                                                            Invariant predicate,
                                                            Invariant consequent,
                                                            boolean iff)
  {
    // No duplicate check because the way it is set up no duplicates should occur:
    // No invariants are duplicates, and since each guarding implication is based
    // off of an existing invariant in a PptSlice, we are guarenteed no duplicate
    // guarding implications exist

    GuardingImplication result = new GuardingImplication(ppt.joiner_view, predicate, consequent, iff);
    return result;
  }

  public boolean isWorthPrinting() {
    return right.isWorthPrinting();
    //return !right.isObvious();
  }

  public boolean justified() {
    return right.justified();
  }
}
