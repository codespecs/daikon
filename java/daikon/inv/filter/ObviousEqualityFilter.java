package daikon.inv.filter;

import daikon.VarInfo;
import daikon.inv.*;
import daikon.inv.filter.*;

class ObviousEqualityFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress obvious equality invariants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    /* [INCR]
    if (IsEqualityComparison.it.accept( invariant )) {
      VarInfo[] variables = invariant.ppt.var_infos; // note: only 2 variables
      for (int i = 0; i < variables.length; i++) {
        if (variables[i].isCanonical()) {
          // Test if equality is "nonobvious".  This test rarely fails, but is
          // necessary for correctness.
          if (variables[i].equalToNonobvious().contains( variables[1-i] )) {
            return false;
          } else {
            return true;
          }
        }
      }
    }
    */
    return false;
  }
}
