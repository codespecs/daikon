package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class ImpliedPostconditionFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress implied postcondition invariants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    // return invariant.isImpliedPostcondition(); // [INCR]
    return false;
  }
}
