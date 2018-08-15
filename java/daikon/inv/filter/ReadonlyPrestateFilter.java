package daikon.inv.filter;

import daikon.PrintInvariants;
import daikon.VarInfo;
import daikon.VarInfo.VarFlags;
import daikon.inv.EqualityComparison;
import daikon.inv.Invariant;
import java.util.logging.Level;

/**
 * Suppress invariants that refer to the prestate value of a field marked with {@link
 * daikon.VarInfo.VarFlags#IS_READONLY}.
 */
public class ReadonlyPrestateFilter extends InvariantFilter {
  @Override
  public String getDescription() {
    return "Suppress invariants indicate that a readonly variable was unmodified";
  }

  /** Boolean. If true, ReadonlyPrestateFilter is initially turned on. */
  public static boolean dkconfig_enabled = true;

  public ReadonlyPrestateFilter() {
    isOn = dkconfig_enabled;
  }

  @Override
  boolean shouldDiscardInvariant(Invariant invariant) {
    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine("\tEntering UnmodRPF.shouldDiscard");
    }

    if (!invariant.isEqualityComparison()) {
      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        PrintInvariants.debugFiltering.fine("\tUnmodRPF thinks this isn't an equality comparison");
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

    if (var1.is_prestate_version(var2) && var1.var_flags.contains(VarFlags.IS_READONLY)) {
      return true;
    } else if (var2.is_prestate_version(var1) && var2.var_flags.contains(VarFlags.IS_READONLY)) {
      return true;
    }
    return false;
  }
}
