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

    /* [INCR]
    Invariant answer = invariant.isImpliedPostcondition();
    if (answer!=null) {
      invariant.discardCode = DiscardCode.implied_post_condition;
      invariant.discardString = "Implied by prestate invariant: "+answer.format()+" at ppt "+answer.ppt.parent.name;
      return true;
    } else {
      return false;
    }
    */ // [INCR]
    return false;
  }
}
