package daikon.inv;

import daikon.*;
import daikon.inv.DiscardInfo;

import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;

import utilMDE.*;

// Here Implication is reimplemented as an extension of the new general
// Joiner class

/**
 * The Implication invariant class is used internally within Daikon to
 * handle invariants that are only true when certain other conditions are
 * also true (splitting).
 **/
public class Implication
  extends Joiner
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  /** the original predicate invariant from its original conditional ppt*/
  public Invariant orig_left;
  /** the original consequent invariant from its original conditional ppt*/
  public Invariant orig_right;

  public Invariant predicate() { return left; }
  public Invariant consequent() { return right; }
  public boolean iff;

  protected Implication(PptSlice ppt, Invariant predicate, Invariant consequent, boolean iff) {
    super(ppt, predicate, consequent);
    this.iff = iff;
  }

  /**
   * Creates a new Implication Invariant from the predicate,
   * consequent and the boolean iff and adds it to the PptTopLevel.
   *
   * @return null if predicate and the consequent are the same, or if
   * the PptTopLevel already contains this Implication.
   **/
   public static Implication makeImplication(PptTopLevel ppt,
                                             Invariant predicate,
                                             Invariant consequent,
                                             boolean iff)
  {
    if (predicate.isSameInvariant(consequent)) {
      PptSplitter.debug.fine ("Not creating implication " + predicate +
                              " ==> " + consequent + " pred == conseq ");
      return null;
    }

    // Don't add this Implication to the program point if the program
    // point already has this implication.  This is slow and dumb; we
    // should use hashing for O(1) check instead.
    for (Iterator i = ppt.joiner_view.invs.iterator(); i.hasNext(); ) {
      Invariant nextInv = (Invariant)i.next();
      if (!(nextInv instanceof Implication)) continue;
      Implication existing = (Implication) nextInv;
      if (existing.iff != iff) continue;
      if (existing.right.getClass() != consequent.getClass()) continue;
      if (existing.left.getClass() != predicate.getClass()) continue;
      // Why not instead check var_info indices plus isSameFormula?
      // Should use PptSlice identity check instead?
      if (! existing.right.format().equals(consequent.format())) continue;
      if (! existing.left.format().equals(predicate.format())) continue;
      PptSplitter.debug.fine ("Not creating implication " + predicate +
                              " ==> " + consequent + "- it already exists as "
                              + existing);
      return null;
    }

    if (PptSplitter.debug.isLoggable (Level.FINE))
      PptSplitter.debug.fine ("Creating implication " + predicate + " ==> "
                            + consequent);
    Implication result = new Implication(ppt.joiner_view, predicate, consequent, iff);
    return result;
  }

  protected double computeConfidence() {
    double pred_conf = orig_left.computeConfidence();
    double cons_conf = orig_right.computeConfidence();
    if ((pred_conf == CONFIDENCE_NEVER)
        || (cons_conf == CONFIDENCE_NEVER)) {
      return CONFIDENCE_NEVER;
    }
    double result = confidence_and(pred_conf, cons_conf);
    log ("Confidence " + result + " " + pred_conf + "/"
                              + cons_conf + " for " + format());
    return result;
  }

  public String repr() {
    return "[Implication: " + left.repr()
      + " => " + right.repr() + "]";
  }

  public String format_using(OutputFormat format) {
    String pred_fmt = left.format_using(format);
    String consq_fmt = right.format_using(format);
    if (format == OutputFormat.DAIKON || format == OutputFormat.JML) {
      String arrow = (iff ? "  <==>  " : "  ==>  "); // "interned"
      return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
    } else if (format == OutputFormat.IOA) {
      String arrow = (iff ? "  <=>  " : "  =>  ");
      return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
    } else if (format == OutputFormat.ESCJAVA) {
      String arrow = (iff ? "  ==  " : "  ==>  "); // "interned"
      return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
    } else if (format == OutputFormat.JAVA) {
      String mid = (iff ? " == " : " || !"); // "interned"
      return "(" + consq_fmt + ")" + mid + "(" + pred_fmt + ")";
    } else if (format == OutputFormat.SIMPLIFY) {
      String cmp = (iff ? "IFF" : "IMPLIES");
      return "(" + cmp + " " + pred_fmt + " " + consq_fmt + ")";
    } else if (format == OutputFormat.DBCJAVA) {
      if ( iff )
        // RRN: I'm not sure if this actually works for DBC;
        //      there seems to be no dedicated biconditional, however.
        // CP: it should work if expressions always return boolean
        //     which I am fixing to make sure of.
        return "((" + pred_fmt + ") == (" + consq_fmt + "))";
      else
        return "(" + pred_fmt + " $implies " + consq_fmt + ")";
    } else {
      return format_unimplemented(format);
    }
  }

  public DiscardInfo isObviousStatically(VarInfo[] vis) {
    Assert.assertTrue (vis.length > 0);
    for (int ii = 0; ii < vis.length; ii++ )
      Assert.assertTrue (vis[ii] != null);
    return orig_right.isObviousStatically(vis);
  }

  public DiscardInfo isObviousDynamically (VarInfo[] vis) {
    Assert.assertTrue (vis.length > 0);
    for (int ii = 0; ii < vis.length; ii++ )
      Assert.assertTrue (vis[ii] != null);
    DiscardInfo di = orig_right.isObviousDynamically (vis);
    if (di != null)
      log ("failed isObviousDynamically with vis = " + VarInfo.toString (vis));
    return (di);
  }


  /**
   * Return true if the right side of the implication and some
   * equality combinations of its member variables are statically
   * obvious.  For example, if a == b, and f(a) is obvious, then so is
   * f(b).  We use the someInEquality (or least interesting) method
   * during printing so we only print an invariant if all its
   * variables are interesting, since a single, static, non
   * interesting occurance means all the equality combinations aren't
   * interesting.
   *
   * This must be overridden for Implication because the right side is
   * the invariant of interest.  The standard version passes the vis
   * from the slice containing the implication itself (slice 0).
   **/
  public DiscardInfo isObviousStatically_SomeInEquality() {
    return orig_right.isObviousStatically_SomeInEquality();
//     DiscardInfo result = isObviousStatically (orig_right.ppt.var_infos);
//     if (result != null) return result;
//     Assert.assertTrue (orig_right.ppt.var_infos.length > 0);
//     for (int ii = 0; ii < orig_right.ppt.var_infos.length; ii++ )
//       Assert.assertTrue (orig_right.ppt.var_infos[ii] != null);
//     return isObviousStatically_SomeInEqualityHelper (orig_right.ppt.var_infos,
//                      new VarInfo[orig_right.ppt.var_infos.length], 0);
  }

  /**
   * Return true if the rightr side of the implication some equality
   * combinations of its member variables are dynamically obvious.
   * For example, a == b, and f(a) is obvious, so is f(b).  We use the
   * someInEquality (or least interesting) method during printing so
   * we only print an invariant if all its variables are interesting,
   * since a single, dynamic, non interesting occurance means all the
   * equality combinations aren't interesting.
   *
   * This must be overridden for Implication because the right side is
   * the invariant of interest.  The standard version passes the vis
   * from the slice containing the implication itself (slice 0).
   **/
  public DiscardInfo isObviousDynamically_SomeInEquality() {
    return orig_right.isObviousDynamically_SomeInEquality();
//     DiscardInfo result = isObviousDynamically (orig_right.ppt.var_infos);
//     if (result != null)
//       return result;
//     return isObviousDynamically_SomeInEqualityHelper (orig_right.ppt.var_infos,
//                                  new VarInfo[right.ppt.var_infos.length], 0);
  }

  public boolean isSameFormula(Invariant other) {
    Implication other_implic = (Implication)other;
    // Guards are necessary because the contract of isSameFormula states
    // that the argument is of the same class as the receiver.
    return (((predicate().getClass() == other_implic.predicate().getClass())
            && predicate().isSameFormula(other_implic.predicate()))
            && ((consequent().getClass() == other_implic.consequent().getClass())
            && consequent().isSameFormula(other_implic.consequent())));
  }

  /* [INCR]
  public boolean hasOnlyConstantVariables() {
    // The old version of this code only looked at the predicate, but
    // it was almost never used, since the OnlyConstantVariablesFilter
    // had a separate check for "inv instanceof Implication" that
    // looked at both the predicate and the consequent. I also can't
    // think of a reason why it would make sense to only check the
    // predicate. -SMcC
    return consequent.hasOnlyConstantVariables()
      || predicate.hasOnlyConstantVariables();
  }
  */ // ... [INCR]

  // An implication is only interesting if both the predicate and
  // consequent are interesting
  public boolean isInteresting() {
    return (predicate().isInteresting() && consequent().isInteresting());
  }

  // If a constant managed to appear in a predicate, that's
  // interesting enough for us.
  public boolean hasUninterestingConstant() {
    return consequent().hasUninterestingConstant();
  }

  public boolean isAllPrestate() {
    return predicate().isAllPrestate() && consequent().isAllPrestate();
  }

  /**
   * Logs a description of the invariant and the specified msg via the
   * log4j logger as described in {@link daikon.Debug#log(Logger, Class, Ppt,
   * VarInfo[], String)}.  Uses the consequent as the logger
   */

  public void log (Logger debug, String msg) {

    right.log (debug, msg + "[for implication " + format() + " ("
               + orig_right.format() + ")]");
  }


 /**
  * Logs a description of the invariant and the specified msg via the
  * log4j logger as described in {@link daikon.Debug#log(Logger, Class, Ppt,
  * VarInfo[], String)}.  Uses the consequent as the logger
  *
  * @return whether or not it logged anything
  */

  public boolean log (String msg) {

    return (right.log (msg + "[for implication " + format() + " ("
               + orig_right.format() + ")]"));
  }


}
