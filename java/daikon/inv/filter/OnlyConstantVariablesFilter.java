package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class OnlyConstantVariablesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants containing only constants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    // return invariant.hasOnlyConstantVariables();
    return false;
  }
}
