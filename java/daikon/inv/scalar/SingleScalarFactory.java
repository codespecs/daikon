package daikon.inv.scalar;

import daikon.*;

import utilMDE.*;

import java.util.*;

public class SingleScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var = ppt.var_infos[0];
    // Assert.assert(! var.rep_type.isArray());
    Assert.assert(var.rep_type == ProglangType.INT);

    Vector result = new Vector();
    if (pass == 1) {
      result.add(OneOfScalar.instantiate(ppt));
    } else if (pass == 2) {
      if (! var.isConstant()) {
        result.add(LowerBound.instantiate(ppt));
        result.add(Modulus.instantiate(ppt));
        result.add(NonModulus.instantiate(ppt));
        result.add(NonZero.instantiate(ppt));
        result.add(UpperBound.instantiate(ppt));
      }
    }
    return result;
  }

  private SingleScalarFactory() {
  }

}
