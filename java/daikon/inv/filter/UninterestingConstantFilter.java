package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class UninterestingConstantFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants with uninteresting literals";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    return invariant.hasUninterestingConstant();
  }
}
