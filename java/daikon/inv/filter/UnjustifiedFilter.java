package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class UnjustifiedFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress unjustified invariants";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    boolean answer =  !invariant.justified();
    if (answer && invariant.discardCode==DiscardInvariant.not_discarded) {
      invariant.discardCode = DiscardInvariant.bad_probability;
      invariant.discardString = "Computed probability " + invariant.getProbability() +
        " > dkconfig_probability_limit==" + Invariant.dkconfig_probability_limit;
    }
    return answer;
  }
}
