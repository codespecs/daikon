package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class UnjustifiedFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress unjustified invariants";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    return ! invariant.justified();
  }
}
