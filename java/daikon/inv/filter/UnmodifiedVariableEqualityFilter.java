package daikon.inv.filter;

import daikon.PrintInvariants;
import daikon.VarInfo;
import daikon.inv.EqualityComparison;
import daikon.inv.Invariant;
import java.util.logging.Level;

/**
 * Suppress invariants that merely indicate that a variable was unmodified. Used only for ESC
 * output.
 */
public class UnmodifiedVariableEqualityFilter extends InvariantFilter {
  @Override
  public String getDescription() {
    return "Suppress invariants that merely indicate that a variable was unmodified";
  }

  /** Boolean. If true, UnmodifiedVariableEqualityFilter is initially turned on. */
  public static boolean dkconfig_enabled = true;

  public UnmodifiedVariableEqualityFilter() {
    isOn = dkconfig_enabled;
  }

  @Override
  boolean shouldDiscardInvariant(Invariant invariant) {
    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine("\tEntering UmVEF.shouldDiscard");
    }

    if (!invariant.isEqualityComparison()) {
      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        PrintInvariants.debugFiltering.fine(
            "\tUnmodVarEqF thinks this isn't an equality comparison");
      }
      return false;
    }

    EqualityComparison comp = (EqualityComparison) invariant;
    VarInfo var1 = comp.var1();
    VarInfo var2 = comp.var2();

    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine(
          "compared " + var1.prestate_name() + " to " + var2.name());
    }

    if (var1.is_prestate_version(var2) || var2.is_prestate_version(var1)) {
      // System.err.printf("prestate: var1 (%s) = var2 (%s)%n", var1, var2);
      return true;
    }

    return false;
  }
}
