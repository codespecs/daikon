package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

public class TwoScalarFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    // Assert.assert((! var1.rep_type.isArray()) && (! var2.rep_type.isArray()));
    Assert.assert(var1.rep_type.equals(ProglangType.INT)
                  && var2.rep_type.equals(ProglangType.INT));

    if (Daikon.check_program_types
        && (! var1.type.comparable(var2.type)))
      return;
    // System.out.println("These have the same type (" + var1.type.format() + "):  "
    //                    + var1.name + " " + var2.name);

    if (pass == 1) {
      IntComparison.instantiate(ppt);
    } else if (pass == 2) {
      for (int i=0; i<2; i++) {
        boolean invert = (i==1);
        VarInfo arg = (invert ? var1 : var2);
        // Skip if the argument is a constant (but not if the result
        // is constant, as we might get something like y=abs(x)).
        if (! arg.isConstant()) {
          for (int j=0; j<Functions.unaryFunctions.length; j++) {
            FunctionUnary.instantiate(ppt, Functions.unaryFunctions[j], invert);
          }
        }
      }
      if ((! var1.isConstant()) && (! var2.isConstant())) {
        LinearBinary.instantiate(ppt);
        // new NonAliased(ppt);
        NonEqual.instantiate(ppt);
      }
    }
  }

  private TwoScalarFactory() {
  }

}
