package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class FewModifiedSamplesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants with few modified samples";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    return invariant.hasFewModifiedSamples();
  }
}
