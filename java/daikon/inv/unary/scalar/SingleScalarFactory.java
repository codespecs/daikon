package daikon.inv.unary.scalar;

import daikon.*;

import utilMDE.*;

import java.util.*;

public final class SingleScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {
    // System.out.println("Ppt arity " + ppt.arity + " " + ppt.name + " " + ppt);
    Assert.assert(ppt.arity == 1);
    VarInfo var = ppt.var_infos[0];
    // Assert.assert(! var.rep_type.isArray());
    Assert.assert(var.rep_type == ProglangType.INT);

    Vector result = new Vector();
    if (pass == 1) {
      result.add(OneOfScalar.instantiate(ppt));
    } else if (pass == 2) {
      if (var.isConstant()) {
        Global.subexact_noninstantiated_invariants += 5;
      } else {
        result.add(NonZero.instantiate(ppt));
        if (var.type.isIntegral()) {
          result.add(LowerBound.instantiate(ppt));
          result.add(Modulus.instantiate(ppt));
          result.add(NonModulus.instantiate(ppt));
          result.add(UpperBound.instantiate(ppt));
        } else {
          // This is suppressed because of types; not sure what global
          // variable to increment for statistics output.
        }
      }
    }
    return result;
  }

  private SingleScalarFactory() {
  }

}
