package daikon.inv.filter;

import daikon.inv.*;
import daikon.VarInfo;
import daikon.PrintInvariants;
import java.util.logging.Level;

/**
 * Suppress invariants that merely indicate that a variable was
 * unmodified.  Used only for ESC output.
 **/
public class UnmodifiedVariableEqualityFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress invariants that merely indicate that a variable was unmodified";
  }

  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine ("\tEntering UmVEF.shouldDiscard\n");
    }

    if (!IsEqualityComparison.it.accept(invariant)) {
      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        PrintInvariants.debugFiltering.fine ("\tUnmodVarEqF thinks this isn't an equality comparison\n");
      }
      return false;
    }

    Comparison comp = (Comparison)invariant;
    VarInfo var1 = comp.var1();
    VarInfo var2 = comp.var2();

    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine ("compared " + var1.name.applyPrestate() + " to " + var2.name.toString() + "\n");
    }

    if (var1.name.applyPrestate().equals(var2.name)) {
      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        PrintInvariants.debugFiltering.fine ("\t(yes...)\n");
      }
      return true ;
    }

    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine ("compared " + var2.name.applyPrestate() + " to " + var1.name.toString() + "\n");
    }

    if (var2.name.applyPrestate().equals(var1.name)) {
      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        PrintInvariants.debugFiltering.fine ("\t(yes...)\n");
      }
      return true;
    }

    return false;
  }
}
