package daikon.inv.string;

import daikon.*;
import utilMDE.*;

import java.util.*;

public class SingleStringFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var = ppt.var_infos[0];
    Assert.assert(var.rep_type == ProglangType.STRING);

    Vector result = new Vector();
    if (pass == 1) {
      result.add(OneOfString.instantiate(ppt));
    } else if (pass == 2) {
      // if (var.isConstant()) {
      //   Global.subexact_noninstantiated_invariants += 5;
      // } else {
      //   LowerBound.instantiate(ppt);
      //   Modulus.instantiate(ppt);
      //   NonModulus.instantiate(ppt);
      //   NonZero.instantiate(ppt);
      //   UpperBound.instantiate(ppt);
      // }
    }
    return result;
  }

  private SingleStringFactory() {
  }

}
