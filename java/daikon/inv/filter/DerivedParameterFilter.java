package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;
import daikon.VarInfo;
import daikon.PrintInvariants;
import daikon.VarInfoAux;

/**
 * Filter for not printing an Invariant if its VarInfos return
 * isDerivedParameterAndUninteresting == true.
 **/
public class DerivedParameterFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress derived parameters that aren't interesting";
  }

  /**
   * Returns true if a given VarInfo in the invariant is derived from a parameter
   * variable and printing it would be uninteresting.  There are 2
   * cases when this would return true for x:
   * <li> If x is a parameter
   * <li> If x is in the form p.a, and it is not true that p = orig(p)
   * In both cases, x.name has to be a postState VarInfoName.  This can
   * only be called after equality invariants are known.
   *
   * (note that this comment was copied pretty much wholesale from
   * VarInfo.java.  I think the sentence about equality invariants
   * means that it can't be called until the equal_to slots are set,
   * which should be true by the time we get here.
   *
   * (I don't think this is applying to equality invariants in quite
   * the way the author intended, but whatever.)
   **/

  boolean shouldDiscardInvariant( Invariant inv ) {
    if (inv.ppt.ppt_name.isExitPoint()) {
      PrintInvariants.debugFiltering.debug("\tconsidering DPF for vars " + inv.ppt.var_infos.toString() + "\n");
      for (int i = 0; i < inv.ppt.var_infos.length; i++) {
        VarInfo vi = inv.ppt.var_infos[i];
        // ppt has to be a PptSlice, not a PptTopLevel
	PrintInvariants.debugFiltering.debug("\tconsidering DPF for " + vi.name.name() + "\n");
	if (vi.isDerivedParamAndUninteresting()) {
	  return true;
        }
      }
    }
    return false;
  }
}
