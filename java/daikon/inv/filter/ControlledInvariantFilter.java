package daikon.inv.filter;

import daikon.PrintInvariants;
import daikon.inv.*;
import java.util.logging.Level;

public class ControlledInvariantFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress controlled invariants [deprecated]";
  }

  // This method finds all the controlling invariants and checks to see if any
  // of them are worth printing.  If any are, then this invariant does not need
  // to be printed.
  boolean shouldDiscardInvariant( Invariant invariant ) {
    return (!isWorthPrinting(invariant)
            && !IsEqualityComparison.it.accept(invariant));
  }

  private boolean isWorthPrinting(Invariant inv) {
    // if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
    //  PrintInvariants.debugFiltering.fine ("\tCalling IWP on " + inv.format() + " for reference" + lineSep);
    //  inv.isWorthPrinting();
    //  PrintInvariants.debugFiltering.fine ("\tdone" + lineSep);
    // }


    if (! isWorthPrinting_sansControlledCheck(inv)) {
      return false;
    }

    // No controller was worth printing
    return true;
  }

  /**
   * Like isWorthPrinting, but doesn't check whether the invariant is controlled.
   **/
  private boolean isWorthPrinting_sansControlledCheck(Invariant inv) {
    if (inv instanceof Implication) {
      Implication impl = (Implication) inv;
      // return isWorthPrinting(impl.predicate()) && isWorthPrinting(impl.consequent());
      // All of these checks are done elsewhere
      return true;
    }

    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      if (!inv.enoughSamples()) {
        PrintInvariants.debugFiltering.fine ("\t\tnot enough samples " + inv.format() + lineSep);
      }
      if (inv.isObvious() != null) {
        PrintInvariants.debugFiltering.fine ("\t\tis obvious " + inv.format() + lineSep);
      }
      if (!inv.justified()) {
        PrintInvariants.debugFiltering.fine ("\t\tnot justified " + inv.format() + lineSep);
      }
    }
    return (InvariantFilters.isWorthPrintingFilter_sansControlledCheck()
            .shouldKeep(inv) == null);
  }
}
