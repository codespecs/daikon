package daikon.inv.filter;

import daikon.VarInfo;
import daikon.inv.*;
import daikon.inv.filter.*;
import utilMDE.Assert;
import daikon.PrintInvariants;

class ObviousEqualityFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress obvious equality invariants [deprecated]";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    /* [INCR]
    if (!IsEqualityComparison.it.accept( invariant )) {
      return false;
    }

    Assert.assertTrue(invariant instanceof Comparison);
    Comparison comp = (Comparison)invariant;

    VarInfo v1 = comp.var1();
    VarInfo v2 = comp.var2();

    Assert.assertTrue(v1.canonicalRep() == v2.canonicalRep());

    VarInfo canonical = v1.canonicalRep();

    if (PrintInvariants.debugFiltering.isDebugEnabled()) {
      PrintInvariants.debugFiltering.debug("\tin ObviousEquality filter\n");
      PrintInvariants.debugFiltering.debug("\t\tv1  is " + v1.name.name() + "\n");
      PrintInvariants.debugFiltering.debug("\t\tv2  is " + v2.name.name() + "\n");
      PrintInvariants.debugFiltering.debug("\t\tcan is " + canonical.name.name() + "\n");
    }

    if (!canonical.equalToNonobvious().contains(v1) && !canonical.equals(v1)) {
      if (PrintInvariants.debugFiltering.isDebugEnabled()) {
	PrintInvariants.debugFiltering.debug("\tit was obvious that " + canonical.name.name() + " == " + v1.name.name() + "\n");
      }
      return true;
    }

    if (!canonical.equalToNonobvious().contains(v2) && !canonical.equals(v2)) {
      if (PrintInvariants.debugFiltering.isDebugEnabled()) {
	PrintInvariants.debugFiltering.debug("\tit was obvious that " + canonical.name.name() + " == " + v2.name.name() + "\n");
      }
      return true;
    }
    */

    return false;

//        VarInfo[] variables = invariant.ppt.var_infos; // note: only 2 variables
//        for (int i = 0; i < variables.length; i++) {
//          if (variables[i].isCanonical()) {
//            // Test if equality is "nonobvious".  This test rarely fails, but is
//            // necessary for correctness.
//            if (variables[i].equalToNonobvious().contains( variables[1-i] )) {
//              return false;
//            } else {
//              return true;
//            }
//          }
//        }
//      }
//      return false;
  }
}
