package daikon.inv.filter;

import daikon.PrintInvariants;
import daikon.inv.*;
import daikon.inv.filter.*;
import java.util.logging.Level;
import java.util.Vector;
import java.util.Iterator;

public class ControlledInvariantFilter extends InvariantFilter {
  public String getDescription() {
    return "Suppress controlled invariants [deprecated]";
  }

  // This method finds all the controlling invariants and checks to see if any
  // of them are worth printing.  If any are, then this invariant does not need
  // to be printed.
  boolean shouldDiscardInvariant( Invariant invariant ) {
    if (!isWorthPrinting(invariant) && !IsEqualityComparison.it.accept(invariant))
      return true;
    else
      return false;
  }

  private boolean isWorthPrinting(Invariant inv)
  {
    // if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
    //  PrintInvariants.debugFiltering.fine ("\tCalling IWP on " + inv.format() + " for reference\n");
    //  inv.isWorthPrinting();
    //  PrintInvariants.debugFiltering.fine ("\tdone\n");
    // }


    if (! isWorthPrinting_sansControlledCheck(inv)) {
      return false;
    }

    // The invariant is worth printing on its own merits, but it may be
    // controlled.  If any (transitive) controller is worth printing, don't
    // print this one.
    // Use _sorted version for reproducibility.  (There's a bug, but I can't find it.)

    /* [INCR]
    Vector contr_invs = inv.find_controlling_invariants_sorted();

    if ((contr_invs.size() == 0) && (PrintInvariants.debugFiltering.isLoggable(Level.FINE))) {
      PrintInvariants.debugFiltering.fine ("\t" + inv.format() + " has no controlling invariants.\n");
    }

    Vector processed = new Vector();
    while (contr_invs.size() > 0) {
      Invariant contr_inv = (Invariant) contr_invs.remove(0);

      processed.add(contr_inv);
      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
        PrintInvariants.debugFiltering.fine ("\tconsidering controlling inv " + contr_inv.format() + "\n");
      }
      if (isWorthPrinting_sansControlledCheck(contr_inv)) {
        // we have a printable controller, so we shouldn't print

        if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
          PrintInvariants.debugFiltering.fine ("\tis controlled by " + contr_inv.format() + " (from " + contr_inv.ppt.parent.name + ")\n");
        }
        inv.discardCode = DiscardCode.control_check;
        inv.discardString = "("+contr_inv.ppt.name+": "+contr_inv.format()+") is worth printing "+
          "and is a controlling Invariant of this";
        return false;
      }
      // find the controlling invs of contr_inv and add them to the
      // working set iff the are not already in it and they have not
      // been processed already
      Iterator iter = contr_inv.find_controlling_invariants().iterator();
      while (iter.hasNext()) {
        Object elt = iter.next();
        if (!processed.contains(elt) && !contr_invs.contains(elt)) {
          contr_invs.add(elt);
        }
      }
    }
    */ // [INCR]

    // No controller was worth printing
    return true;
  }

  /**
   * Like isWorthPrinting, but doesn't check whether the invariant is controlled.
   **/
  private boolean isWorthPrinting_sansControlledCheck(Invariant inv) {
    if (inv instanceof Implication) {
      Implication impl = (Implication) inv;
      return isWorthPrinting(impl.predicate()) && isWorthPrinting(impl.consequent());
    }

    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      if (inv.hasFewModifiedSamples()) {
        PrintInvariants.debugFiltering.fine ("\t\thas few modified samples " + inv.format() + "\n");
      }
      if (!inv.enoughSamples()) {
        PrintInvariants.debugFiltering.fine ("\t\tnot enough samples " + inv.format() + "\n");
      }
      /* [INCR]
      if (inv.hasNonCanonicalVariable()) {
        PrintInvariants.debugFiltering.fine ("\t\thas non canonical var " + inv.format() + "\n");
      }
      if (inv.hasOnlyConstantVariables()) {
        PrintInvariants.debugFiltering.fine ("\t\thas only constant vars " + inv.format() + "\n");
      }
      */ // [INCR]
      if (inv.isObvious().shouldDiscard()) {
        PrintInvariants.debugFiltering.fine ("\t\tis obvious " + inv.format() + "\n");
      }
      if (!inv.justified()) {
        PrintInvariants.debugFiltering.fine ("\t\tnot justified " + inv.format() + "\n");
      }
      /* [INCR]
      if (!inv.isWorthPrinting_PostconditionPrestate()) {
        PrintInvariants.debugFiltering.fine ("\t\tisn't worth printing postcond/prestate " + inv.format() + "\n");
      }
      */ // [INCR]
    }
    return (InvariantFilters.isWorthPrintingFilter_sansControlledCheck().shouldKeep(inv));
  }
}
