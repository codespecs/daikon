package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

public final class TwoScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    // Assert.assert((! var1.rep_type.isArray()) && (! var2.rep_type.isArray()));
    Assert.assert((var1.rep_type == ProglangType.INT)
                  && (var2.rep_type == ProglangType.INT));

    if (Daikon.check_program_types
        && (! var1.type.comparable(var2.type))) {
      // System.out.println("These have different types: :  "
      //                    + var1.name + " (" + var1.type.format() + ") " + var2.name +  " (" + var2.type.format() + ") ");
      return null;
    }
    // System.out.println("These have the same type (" + var1.type.format() + "):  "
    //                    + var1.name + " " + var2.name);

    Vector result = new Vector();
    if (pass == 1) {
      result.add(IntComparison.instantiate(ppt));
    } else if (pass == 2) {
      for (int i=0; i<2; i++) {
        boolean invert = (i==1);
        VarInfo argvar = (invert ? var1 : var2);
        VarInfo resultvar = (invert ? var2 : var1);
        // Skip if the argument is a constant (but not if the result
        // is constant, as we might get something like y=abs(x)).
        // On second thought, also skip if the result is constant.
        if (argvar.isConstant() || resultvar.isConstant()) {
          Global.subexact_noninstantiated_invariants += Functions.unaryFunctions.length;
        } else {
          for (int j=0; j<Functions.unaryFunctions.length; j++) {
            result.add(FunctionUnary.instantiate(ppt, Functions.unaryFunctionNames[j], Functions.unaryFunctions[j], invert));
          }
        }
      }
      if (var1.isConstant() || var2.isConstant()) {
        Global.subexact_noninstantiated_invariants += 2;
      } else {
        result.add(LinearBinary.instantiate(ppt));
        // Perhaps do not instantiate unless the variables have
        // the same type; in particular, nonequal for Object variables
        // is not so likely to be of interest.
        result.add(NonEqual.instantiate(ppt));
      }
    }
    return result;
  }

  private TwoScalarFactory() {
  }

}
