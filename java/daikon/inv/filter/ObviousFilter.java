package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class ObviousFilter extends InvariantFilter {
  public String getDescription() { return "Suppress obvious invariants"; }
  boolean shouldDiscardInvariant( Invariant invariant ) {
    return invariant.isObvious();
  }
}
