package daikon.inv.filter;

import daikon.inv.*;
import daikon.PrintInvariants;
import java.util.logging.Level;

/**
 * Filter for not printing invariants suppressed during checking.
 **/
public class SuppressionFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants that aren't checked during run";
  }

  boolean shouldDiscardInvariant( Invariant inv ) {
    if (inv instanceof Implication) {
      Implication imp = (Implication) inv;
      if (imp.consequentSuppressor() != null) {
        return (true);
      }
    }
    if (inv.getSuppressor() != null) {
      if (inv.logOn() || PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        inv.log (PrintInvariants.debugFiltering,
                 "suppressed by: " + inv.getSuppressor());
      }
      return true;
    }
    return false;
  }
}
