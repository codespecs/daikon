package daikon.inv.filter;

import daikon.PptTopLevel;
import daikon.VarInfo;
import daikon.inv.*;
import daikon.inv.filter.*;

import java.util.*;

class SimplifyFilter extends InvariantFilter {
  String description = "Eliminate invariants based on Simplify (slow)";

  public String getDescription() {
    return description;
  }

  // Need this reference for MyTester
  InvariantFilters filters;

  public SimplifyFilter( InvariantFilters filters ) {
    this.filters = filters;
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    // Rerun simplify on this Ppt
    PptTopLevel topLevel = (PptTopLevel) invariant.ppt.parent;
    topLevel.redundant_invs = new HashSet();
    topLevel.mark_implied_via_simplify( new MyTester());
    Set redundantInvariants = topLevel.redundant_invs;

    // See if this invariant appears in redundant_invs:
    // For regular invariants, the invariant itself is stored in redundant_invs.
    // For Equality invariants, their canonical variable is stored in
    // redundant_invs.  (This might change in the future to their non-canonical
    // variable, which is why all variables are checked.)

    // Can't do "if (invariant instanceof Equality)" because
    // InvariantFilters.addEqualityInvariants() hasn't been called yet, so
    // equality comparison invariants haven't been converted to Equality
    // invariants yet.
    if (IsEqualityComparison.it.accept( invariant )) {
	VarInfo[] variables = invariant.ppt.var_infos;
	for (int i = 0; i < variables.length; i++)
	    if (redundantInvariants.contains( variables[i] ))
		return true;
	return false;
    }
    else {
      return redundantInvariants.contains( invariant );
    }
  }

  class MyTester implements daikon.PptTopLevel.SimplifyInclusionTester {
    // mark_implied_via_simplify() uses this method to find out which other
    // invariants in a Ppt should be considered.  (Only invariants which pass
    // the filter tests are considered.)  This method uses InvariantFilters to
    // determine if an invariant passes the filter tests.  This method
    // temporarily turns SimplifyFilter off so that we don't get into an
    // infinite loop.
    public boolean include( Invariant invariant ) {

      // This needs to be the same filters object that was passed into this
      // class's constructor.  Ideally, this method would take filters object as
      // an argument, but for now, this should be the same object.
      boolean simplifyFilterOn = filters.getFilterSetting( description );
      filters.changeFilterSetting( description, false );
      boolean shouldKeep = filters.shouldKeep( invariant );
      filters.changeFilterSetting( description, simplifyFilterOn );
      return shouldKeep;
    }
  }
}
