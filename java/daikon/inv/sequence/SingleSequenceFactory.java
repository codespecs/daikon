package daikon.inv.sequence;

import daikon.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public class SingleSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt) {
    // return (UnaryDerivation)new SequenceFirst(vi);

    // Not really the right place for this test
    if (ppt.var_infos[0].type.dimensions() == 0)
      return;

    EltIntComparison.instantiate(ppt);
  }

  private SingleSequenceFactory() {
  }

}
