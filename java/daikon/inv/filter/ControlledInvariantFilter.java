package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;
import java.util.Vector;

class ControlledInvariantFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress controlled invariants";
  }

  // We need to have a reference to an InvariantFilters object, so we know whether or not
  // to keep any controlling invariant that we find.
  InvariantFilters filters;

  public ControlledInvariantFilter( InvariantFilters filters ) {
    this.filters = filters;
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    Vector controllingInvariants = invariant.find_controlling_invariants();
    for (int i=0; i < controllingInvariants.size(); i++) {
      Invariant controllingInvariant = (Invariant) controllingInvariants.get( i );
      if (filters.shouldKeep( controllingInvariant ))
	return true;
    }
    return false;
  }
}
