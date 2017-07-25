package daikon.inv.filter;

import daikon.Daikon;
import daikon.inv.*;

public class SimplifyFilter extends InvariantFilter {
  static String description = "Eliminate invariants based on Simplify (slow)";

  @Override
  public String getDescription() {
    return description;
  }

  /** Boolean. If true, SimplifyFilter is initially turned on. */
  public static boolean dkconfig_enabled = true;

  public SimplifyFilter() {
    isOn = dkconfig_enabled;
  }

  @Override
  boolean shouldDiscardInvariant(Invariant invariant) {
    if (Daikon.suppress_redundant_invariants_with_simplify
        && invariant.ppt.parent.redundant_invs.contains(invariant)) {
      return true;
    }
    return false;
  }
}
