package daikon.inv.filter;

import daikon.inv.*;

public class VariableFilter extends InvariantFilter {
  // This method is more for the property filters, but we need to implement
  // it because it's abstract.
  @Override
  public String getDescription() {
    return "Variable filter on '" + variable + "'";
  }

  String variable;

  public VariableFilter(String variable) {
    this.variable = variable;
  }

  public String getVariable() {
    return variable;
  }

  @Override
  boolean shouldDiscardInvariant(Invariant invariant) {
    return !invariant.usesVar(variable);
  }
}
