package daikon.inv.filter;

import daikon.inv.*;

public class EnoughSamplesFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants which do not have enough samples";
  }

  // This does not discard equality invariants because (I'm told) that
  // there are likely to be meaningful ones which do not have many
  // modified samples.
  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (IsEqualityComparison.it.accept( invariant )) {
      return false;
    }
    invariant.log ("enoughSamples = " + invariant.enoughSamples());
    return !invariant.enoughSamples();
   }
}
