package daikon.inv.binary.twoString;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

public final class TwoStringFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    // Assert.assert((! var1.rep_type.isArray()) && (! var2.rep_type.isArray()));
    Assert.assert((var1.rep_type == ProglangType.STRING)
                  && (var2.rep_type == ProglangType.STRING));

    if (Daikon.check_program_types
        && (! var1.type.comparable(var2.type))) {
      // System.out.println("These have different program types: :  "
      //                    + var1.name + " (" + var1.type.format() + ") " + var2.name +  " (" + var2.type.format() + ") ");
      return null;
    }
    // System.out.println("These have comparable program types: :  "
    //                    + var1.name + " (" + var1.type.format() + ") " + var2.name +  " (" + var2.type.format() + ") ");
    if (! Daikon.ignore_comparability) {
      if (! VarComparability.compatible(var1, var2)) {
        return null;
      }
    }

    Vector result = new Vector();
    if (pass == 1) {
      result.add(StringComparison.instantiate(ppt));
    } else if (pass == 2) {
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
      //   result.add(NonEqual.instantiate(ppt));
      // }
    }
    return result;
  }

  private TwoStringFactory() {
  }

}
