package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

public class OnlyConstantVariablesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants containing only constants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (IsEqualityComparison.it.accept(invariant)) {
      return false;
    }

    return false;

    /* [INCR]
    boolean answer = invariant.hasOnlyConstantVariables();
    if (answer) {
      invariant.discardCode = DiscardCode.only_constant_vars;
      invariant.discardString = "Every variable in this invariant is constant.";
      return true;
    } else return false;
    */ // INCR
  }
}
