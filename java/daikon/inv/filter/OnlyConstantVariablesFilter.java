package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class OnlyConstantVariablesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants containing only constants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (IsEqualityComparison.it.accept(invariant)) {
      return false;
    }

    return false;

    /* [INCR]
    if (invariant instanceof Implication) {
      Implication impl = (Implication) invariant;
      return impl.consequent().hasOnlyConstantVariables() || impl.predicate().hasOnlyConstantVariables();
    }

    return invariant.hasOnlyConstantVariables();
    */ // INCR
  }
}
