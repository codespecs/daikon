package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

public class UnjustifiedFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress unjustified invariants";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    return !invariant.justified();
  }
}
