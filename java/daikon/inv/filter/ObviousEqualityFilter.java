package daikon.inv.filter;

import daikon.VarInfo;
import daikon.inv.*;

public class ObviousEqualityFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress obvious equality invariants [deprecated]";
  }

  // Print the variable, its canonical variable, and whether they can
  // be mising.
  private void print_var(String name, VarInfo vi) {
    System.out.println(name + "=" + vi.name.name());
    System.out.println("  canBeMissing=" + vi.canBeMissing);
    System.out.println("  repr=" + vi.repr());
  }


  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (!(invariant instanceof Equality)) return false;

    Equality eq = (Equality) invariant;

    if (eq.getVars().size() < 2) return true;

    return false;


  }
}
