package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;
import java.util.Vector;

class ControlledInvariantFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress controlled invariants [deprecated]";
  }

  // We need to have a reference to an InvariantFilters object, so we
  // know whether or not to keep any controlling invariant that we
  // find.
  InvariantFilters filters;

  public ControlledInvariantFilter( InvariantFilters filters ) {
    this.filters = filters;
  }

  // This method finds all the controlling invariants and checks to see if any
  // of them are worth printing.  If any are, then this invariant does not need
  // to be printed.
  boolean shouldDiscardInvariant( Invariant invariant ) {
    /* // [INCR]
    if (invariant.find_controlling_invariants().size() == 0)
      return false;

    // Get all the controlling invariants, not just the parent ones.
    Vector controllingInvariants = getAllControllingInvariants( invariant );
    //    if (invariant.find_controlling_invariants().size() != controllingInvariants.size())
    //      System.out.println( "****   " + invariant.find_controlling_invariants().size() + " -> " + controllingInvariants.size() + "   ****");
    for (int i=0; i < controllingInvariants.size(); i++) {
      Invariant controllingInvariant = (Invariant) controllingInvariants.get( i );
      // Temporarily turn off Simplify filter.  If a controlling invariant is
      // eliminated by Simplify, then the controlled invariant will also be
      // eliminated.  Not recursively calling Simplify saves a lot of time.
      boolean simplifyFilterOn = filters.getFilterSetting( SimplifyFilter.description );
      filters.changeFilterSetting( SimplifyFilter.description, false );
      boolean shouldKeep = filters.shouldKeep( controllingInvariant );
      filters.changeFilterSetting( SimplifyFilter.description, simplifyFilterOn );
      if (shouldKeep)
	return true;
    }
    */
    return false;
  }

  // This recursive method returns all controlling invariants, including
  // parents, parents of parents, etc.
  /* [INCR]
  Vector getAllControllingInvariants( Invariant invariant ) {
    Vector parents = invariant.find_controlling_invariants();
    Vector allControllingInvariants = new Vector();
    if (parents.size() == 0)	// base case: just return this invariant
      allControllingInvariants.add( invariant );
    else			// aggregate all the parents' trees
      for (int i=0; i < parents.size(); i++) {
	Invariant parent = (Invariant) parents.get( i );
	allControllingInvariants.addAll( getAllControllingInvariants( parent ));
      }
    return allControllingInvariants;
  }
  */
}
