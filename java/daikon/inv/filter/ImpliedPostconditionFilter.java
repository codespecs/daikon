package daikon.inv.filter;

import daikon.inv.*;

public class ImpliedPostconditionFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress implied postcondition invariants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (IsEqualityComparison.it.accept(invariant)) {
      return false;
    }

    return false;
  }
}
