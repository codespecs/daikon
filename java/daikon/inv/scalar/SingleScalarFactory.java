package daikon.inv.scalar;

import daikon.*;
import utilMDE.*;

public class SingleScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    Assert.assert(! ppt.var_infos[0].type.isArray());

    if (pass == 1) {
      OneOfScalar.instantiate(ppt);
    } else if (pass == 2) {
      LowerBound.instantiate(ppt);
      Modulus.instantiate(ppt);
      NonModulus.instantiate(ppt);
      NonZero.instantiate(ppt);
      UpperBound.instantiate(ppt);
    }
  }

  private SingleScalarFactory() {
  }

}
