package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import org.apache.log4j.Category;

import java.util.*;

public final class TwoScalarFactory {

  /**
   * Debugging tracer.
   *
   **/
  public static final Category debug = Category.getInstance("daikon.inv.binary.twoScalar.TwoScalarFactory");


  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {


    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    if (debug.isDebugEnabled()) {
      debug.debug ("Trying to instantiate for " + ppt.ppt_name + ": " + var1.name  + " and " + var2.name);
    }


    // Assert.assert((! var1.rep_type.isArray()) && (! var2.rep_type.isArray()));
    Assert.assert(var1.rep_type.isScalar());
    Assert.assert(var2.rep_type.isScalar());

    if (! var1.compatible(var2)) {
      debug.debug ("Not comparable, returning");
      return null;
    }

    // In V3, we are more ambitious and will do inference over static
    // constants.  In any case, this test belongs in PptTopLevel anyway.
    // if (var1.isStaticConstant() || var2.isStaticConstant()) {
    //   return null;
    // }

    boolean integral = var1.file_rep_type.isIntegral() && var2.file_rep_type.isIntegral();

    Vector result = new Vector();
    result.add(IntEqual.instantiate(ppt));
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

    return result;
  }

  private TwoScalarFactory() {
  }

}
