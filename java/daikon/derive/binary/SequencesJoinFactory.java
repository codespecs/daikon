package daikon.derive.binary;

import daikon.*;
import daikon.inv.binary.twoScalar.*; // for IntComparison
import daikon.inv.unary.scalar.*; // for LowerBound

import utilMDE.*;
import org.apache.log4j.Category;
import java.util.*;

/**
 * Factory for SequencesJoin derived variables.
 *
 **/

public final class SequencesJoinFactory  extends BinaryDerivationFactory {

  /**
   * Debug tracer
   **/

  public static final Category debug =
    Category.getInstance ("daikon.derive.binary.SequencesJoinFactory");

  public BinaryDerivation[] instantiate(VarInfo var1, VarInfo var2) {

    boolean enabled = SequencesJoin.dkconfig_enabled;
    if (!enabled) return null;

    if (!(var1.rep_type.isArray()) ||
	!(var2.rep_type.isArray())) {
      return null;
    }

    /**    if (!(var1.rep_type == ProglangType.INT_ARRAY)  ||
	   !(var2.rep_type == ProglangType.INT_ARRAY)) return null;**/

    if (var1.derived != null || var2.derived != null) {
      // From derived variables.  Don't derive.
      return null;
    }

    if (var1.name.equals(var2.name)) return null;

    if (!(var1.name instanceof VarInfoName.Field) ||
	!(var2.name instanceof VarInfoName.Field))   return null;
    
    VarInfoName.Field name1 = (VarInfoName.Field) var1.name;
    VarInfoName.Field name2 = (VarInfoName.Field) var2.name;

    if (!name1.term.equals(name2.term)) return null;

    // Now we finally can derive

    // Assert.assert(var1.isCanonical()); // [INCR]
    // Assert.assert(var2.isCanonical()); // [INCR]

    if (debug.isDebugEnabled()) {
      debug.debug (var1.ppt + ": " + var1.name + " and " +
		   var2.name + " are worth deriving from");
    }

    return new BinaryDerivation[] {
      new SequencesJoin (var1, var2),
    };
  }

}


