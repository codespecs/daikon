package daikon.inv.string;

import daikon.*;
import utilMDE.*;

public class SingleStringFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    VarInfo var = ppt.var_infos[0];
    Assert.assert(! var.type.isArray());

    if (pass == 1) {
      OneOfString.instantiate(ppt);
    } else if (pass == 2) {
      // if (! var.isConstant()) {
      //   LowerBound.instantiate(ppt);
      //   Modulus.instantiate(ppt);
      //   NonModulus.instantiate(ppt);
      //   NonZero.instantiate(ppt);
      //   UpperBound.instantiate(ppt);
      // }
    }
  }

  private SingleStringFactory() {
  }

}
