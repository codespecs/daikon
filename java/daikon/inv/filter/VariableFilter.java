package daikon.inv.filter;

import daikon.inv.*;
import daikon.inv.filter.*;

class VariableFilter extends InvariantFilter {
  // This method is more for the property filters, but we need to implement
  // it because it's abstract.
  public String getDescription() {
    return "Variable filter on '" + variable + "'";
  }

  String variable;

  public VariableFilter( String variable ) {
    this.variable = variable;
  }

  public String getVariable() {
    return variable;
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (invariant.usesVar( variable ))
      return false;
    else {
      invariant.discardCode = DiscardInvariant.var_filtered;
      invariant.discardString = "Discard because Invariant does not have variable " + variable;
      return true;
    }
  }
}
