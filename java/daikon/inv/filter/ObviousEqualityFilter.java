package daikon.inv.filter;

import daikon.VarInfo;
import daikon.inv.*;
import daikon.inv.filter.*;
import daikon.inv.binary.twoSequence.*;
import daikon.PrintInvariants;

import utilMDE.Assert;

class ObviousEqualityFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress obvious equality invariants [deprecated]";
  }

  // Print the variable, its canonical variable, and whether they can be mising.
  private void print_var(String name, VarInfo vi) {
    System.out.println(name + "=" + vi.name.name());
    // [INCR] System.out.println("  canonical=" + vi.canonicalRep().name.name());
    System.out.println("  canBeMissing=" + vi.canBeMissing
                       /* [INCR] + "," + vi.canonicalRep().canBeMissing */);
    System.out.println("  repr=" + vi.repr());
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

    /// Debugging
    // print_var("v1", v1);
    // print_var("v2", v2);
    // if (! ((v1.canonicalRep() == v2.canonicalRep())
    //        || v1.canBeMissing || v2.canBeMissing)) {
    //   daikon.PptTopLevel ppt = invariant.ppt.parent;
    //   for (int i=0; i<ppt.var_infos.length; i++) {
    //     VarInfo vi = ppt.var_infos[i];
    //     print_var("var_infos[" + i + "]", vi);
    //   }
    // }

    Assert.assertTrue((v1.canonicalRep() == v2.canonicalRep())
                      || v1.canBeMissing || v2.canBeMissing);

    VarInfo canonical = v1.canonicalRep();

    if (PrintInvariants.debugFiltering.isDebugEnabled()) {
      PrintInvariants.debugFiltering.debug("in ObviousEquality filter\n");
      PrintInvariants.debugFiltering.debug("\tv1  is " + v1.name.name() + "\n");
      PrintInvariants.debugFiltering.debug("\tv2  is " + v2.name.name() + "\n");
      PrintInvariants.debugFiltering.debug("\tcan is " + canonical.name.name() + "\n");
    }

    if (!canonical.equalToNonobvious().contains(v1) && !canonical.equals(v1)) {
      if (PrintInvariants.debugFiltering.isDebugEnabled()) {
        PrintInvariants.debugFiltering.debug("it was obvious that " + canonical.name.name() + " == " + v1.name.name() + "\n");
      }
      return true;
    }

    if (!canonical.equalToNonobvious().contains(v2) && !canonical.equals(v2)) {
      if (PrintInvariants.debugFiltering.isDebugEnabled()) {
        PrintInvariants.debugFiltering.debug("it was obvious that " + canonical.name.name() + " == " + v2.name.name() + "\n");
      }
      return true;
    }

    if ((invariant instanceof SeqComparison)
        || (invariant instanceof SeqComparisonFloat)) {
      VarInfo super1 = v1.isDerivedSubSequenceOf();
      VarInfo super2 = v2.isDerivedSubSequenceOf();
      if ((super1 != null) && (super2 != null) && (super1 == super2)) {
        return true;
      }
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
