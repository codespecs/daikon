package daikon.inv.unary.scalar;

import daikon.*;

import utilMDE.*;

import java.util.*;

public final class SingleFloatFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {
    // System.out.println("Ppt arity " + ppt.arity + " " + ppt.name + " " + ppt);
    Assert.assert(ppt.arity == 1);
    VarInfo var = ppt.var_infos[0];
    // Assert.assert(! var.rep_type.isArray());
    Assert.assert(var.rep_type == ProglangType.DOUBLE);

    Vector result = new Vector();
    if (pass == 1) {
    } else if (pass == 2) {
    }
    return result;
  }

  private SingleFloatFactory() {
  }

}
