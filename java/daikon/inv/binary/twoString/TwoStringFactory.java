package daikon.inv.binary.twoString;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

public final class TwoStringFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {

    if (Debug.logOn())
      ppt.log ("Considering two string instantiation");

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    // Assert.assertTrue((! var1.rep_type.isArray()) && (! var2.rep_type.isArray()));
    Assert.assertTrue((var1.rep_type == ProglangType.STRING)
                  && (var2.rep_type == ProglangType.STRING));

    if (! var1.compatible(var2)) {
      if (Debug.logOn())
        ppt.log ("No instantiate, vars not compatible");
      return null;
    }

    Vector result = new Vector();
    // result.add(StringComparison.instantiate(ppt)); // FIXME for equality
    result.add(StringEqual.instantiate(ppt));
    result.add(StringNonEqual.instantiate(ppt));
    result.add(StringLessThan.instantiate(ppt));
    result.add(StringLessEqual.instantiate(ppt));
    result.add(StringGreaterThan.instantiate(ppt));
    result.add(StringGreaterEqual.instantiate(ppt));

      /// copied from TwoScalarFactory.
      // for (int i=0; i<2; i++) {
      //   boolean invert = (i==1);
      //   VarInfo argvar = (invert ? var1 : var2);
      //   VarInfo resultvar = (invert ? var2 : var1);
      //   // Skip if the argument is a constant (but not if the result
      //   // is constant, as we might get something like y=abs(x)).
      //   // On second thought, also skip if the result is constant.
      //   if (argvar.isConstant() || resultvar.isConstant()) {
      //     Global.subexact_noninstantiated_invariants += Functions.unaryFunctions.length;
      //   } else {
      //     for (int j=0; j<Functions.unaryFunctions.length; j++) {
      //       result.add(FunctionUnary.instantiate(ppt, Functions.unaryFunctionNames[j], Functions.unaryFunctions[j], invert));
      //     }
      //   }
      // }

      // if (var1.isConstant() || var2.isConstant()) {
      //   Global.subexact_noninstantiated_invariants += 1;
      // } else {
      //   result.add(StringNonEqual.instantiate(ppt));
      // }

    return result;
  }

  private TwoStringFactory() {
  }

}
