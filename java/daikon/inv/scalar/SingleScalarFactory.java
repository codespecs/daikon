package daikon.inv.scalar;

import daikon.*;
import utilMDE.*;

public class SingleScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    VarInfo var = ppt.var_infos[0];
    // Assert.assert(! var.rep_type.isArray());
    Assert.assert(var.rep_type.equals(ProglangType.INT));

    if (pass == 1) {
      OneOfScalar.instantiate(ppt);
    } else if (pass == 2) {
      if (! var.isConstant()) {
        LowerBound.instantiate(ppt);
        Modulus.instantiate(ppt);
        NonModulus.instantiate(ppt);
        NonZero.instantiate(ppt);
        UpperBound.instantiate(ppt);
      }
    }
  }

  private SingleScalarFactory() {
  }

}
