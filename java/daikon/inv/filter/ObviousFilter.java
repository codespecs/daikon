package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class ObviousFilter extends InvariantFilter {
  public String getDescription() { return "Suppress obvious invariants"; }
  boolean shouldDiscardInvariant( Invariant invariant ) {
    boolean answer = invariant.isObvious();
    if (answer && invariant.discardString.equals(""))
      invariant.discardString = invariant.getClass().getName()+": Fix me, "+
        "discarded because isObvious()";
    return answer;
  }
}
