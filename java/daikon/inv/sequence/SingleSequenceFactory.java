package daikon.inv.sequence;

import daikon.*;

import utilMDE.*;

public class SingleSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {
    // return (UnaryDerivation)new SequenceFirst(vi);

    // Not really the right place for this test
    Assert.assert(ppt.var_infos[0].type.isArray());

    if (pass == 1) {
      OneOfSequence.instantiate(ppt);
    } else if (pass == 2) {
      EltIntComparison.instantiate(ppt);
    }
  }

  private SingleSequenceFactory() {
  }

}
