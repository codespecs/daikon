package daikon.inv.scalar;

import daikon.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public class SingleScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt) {
    // return (UnaryDerivation)new SequenceFirst(vi);

    // Not really the right place for this test
    if (!(ppt.var_infos[0].type.dimensions() == 0))
      return;

    new LowerBound(ppt);
    new Modulus(ppt);
    new NonModulus(ppt);
    new NonZero(ppt);
    new UpperBound(ppt);
  }

  private SingleScalarFactory() {
  }

}
