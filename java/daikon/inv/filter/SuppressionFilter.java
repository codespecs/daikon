package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;
import daikon.VarInfo;
import daikon.PrintInvariants;
import daikon.VarInfoAux;

/**
 * Filter for not printing invariants suppressed during checking.
 **/
public class SuppressionFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants that aren't checked during run";
  }

  boolean shouldDiscardInvariant( Invariant inv ) {
    if (inv.getSuppressor() != null) {
      if (PrintInvariants.debugFiltering.isDebugEnabled()) {
        PrintInvariants.debugFiltering.debug ("  suppressed by: " + inv.getSuppressor());
      }
      return true;
    }
    return false;
  }
}
