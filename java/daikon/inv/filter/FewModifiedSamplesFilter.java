package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class FewModifiedSamplesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants with few modified samples";
  }

  // This does not discard equality invariants because (I'm told) that
  // there are likely to be meaningful ones which do not have many
  // modified samples.
  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (IsEqualityComparison.it.accept( invariant )) {
      return false;
    }
    boolean answer = invariant.hasFewModifiedSamples();
    if (answer && invariant.discardString.equals(""))
      invariant.discardString = invariant.getClass().getName()+": Fix me, "+
        "discarded for few modified samples";
    return answer;
  }
}
