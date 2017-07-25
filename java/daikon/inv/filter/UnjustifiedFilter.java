package daikon.inv.filter;

import daikon.inv.*;

public class UnjustifiedFilter extends InvariantFilter {
  @Override
  public String getDescription() {
    return "Suppress unjustified invariants";
  }

  /** Boolean. If true, UnjustifiedFilter is initially turned on. */
  public static boolean dkconfig_enabled = true;

  public UnjustifiedFilter() {
    isOn = dkconfig_enabled;
  }

  @Override
  boolean shouldDiscardInvariant(Invariant invariant) {
    return !invariant.justified();
  }
}
