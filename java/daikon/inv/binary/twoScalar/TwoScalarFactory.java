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
    Assert.assert(var1.rep_type.isScalar());
    Assert.assert(var2.rep_type.isScalar());

    if (! var1.compatible(var2))
      return null;

    boolean integral = var1.type.isIntegral() && var2.type.isIntegral();

    Vector result = new Vector();
    if (pass == 1) {
      result.add(IntEqual.instantiate(ppt));
    } else if (pass == 2) {
      if (var1.isConstant() || var2.isConstant()) {
        Global.subexact_noninstantiated_invariants += 2;
	Global.subexact_noninstantiated_invariants += Functions.unaryFunctions.length;
      } else {
        result.add(IntNonEqual.instantiate(ppt));
        result.add(IntLessThan.instantiate(ppt));
        result.add(IntLessEqual.instantiate(ppt));
        result.add(IntGreaterThan.instantiate(ppt));
        result.add(IntGreaterEqual.instantiate(ppt));
	// Skip LineayBinary and FunctionUnary unless vars are integral
	if (!integral) {
	  Global.subexact_noninstantiated_invariants += 1;
	  Global.subexact_noninstantiated_invariants += Functions.unaryFunctions.length;
	} else {
	  result.add(LinearBinary.instantiate(ppt));
	  for (int i=0; i<2; i++) {
	    boolean invert = (i==1);
	    for (int j=0; j<Functions.unaryFunctions.length; j++) {
	      result.add(FunctionUnary.instantiate(ppt, Functions.unaryFunctionNames[j], Functions.unaryFunctions[j], invert));
	    }
	  }
	}
      }
    }
    return result;
  }

  private TwoScalarFactory() {
  }

}
