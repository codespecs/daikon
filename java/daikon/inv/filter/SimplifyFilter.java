package daikon.inv.filter;

import daikon.PptTopLevel;
import daikon.VarInfo;
import daikon.Daikon;
import daikon.inv.*;
import daikon.inv.filter.*;

import java.util.*;

class SimplifyFilter extends InvariantFilter {
  static String description = "Eliminate invariants based on Simplify (slow)";
  InvariantFilters filters;     // need this reference for MyTester
  PptTopLevel previousTopLevel; // hack for speed, used in shouldDiscardCode()

  public String getDescription() {
    return description;
  }

  public SimplifyFilter( InvariantFilters filters ) {
    this.filters = filters;
    previousTopLevel = null;
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (Daikon.suppress_redundant_invariants_with_simplify &&
        invariant.ppt.parent.redundant_invs.contains(invariant)) {
      invariant.discardCode = DiscardCode.obvious;
      invariant.discardString = "This Invariant was filtered due to Simplify.";
      return (true);
    }

    return false;

//      PptTopLevel topLevel = invariant.ppt.parent;

//      // Using previousTopLevel is a hack for speed.  If the current topLevel is
//      // the same as previousTopLevel, then we are looking at an invariant from
//      // the same Ppt as last time.  We assume that the filters haven't changed
//      // since the last time we ran Simplify, so we'll use the results (ie,
//      // redundant_invs) from last time.  This way, Simplify is only run once per
//      // Ppt rather than once per invariant.
//      if (topLevel != previousTopLevel) {     // run Simplify on this Ppt
//        topLevel.redundant_invs = new HashSet();
//        topLevel.mark_implied_via_simplify(filters.getPptMap(), new MyTester());
//      }
//      previousTopLevel = topLevel;
//      Set redundantInvariants = topLevel.redundant_invs;

//      // See if this invariant appears in redundant_invs:
//      // For regular invariants, the invariant itself is stored in redundant_invs.
//      // For Equality invariants, their canonical variable is stored in
//      // redundant_invs.  (This might change in the future to their non-canonical
//      // variable, which is why all variables are checked.)

//      // Can't do "if (invariant instanceof Equality)" because
//      // InvariantFilters.addEqualityInvariants() hasn't been called yet, so
//      // equality comparison invariants haven't been converted to Equality
//      // invariants yet.
//      if (IsEqualityComparison.it.accept( invariant )) {
//      VarInfo[] variables = invariant.ppt.var_infos;
//      for (int i = 0; i < variables.length; i++)
//          if (redundantInvariants.contains( variables[i] ))
//              return true;
//      return false;
//      }
//      else {
//        return redundantInvariants.contains( invariant );
//      }
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
