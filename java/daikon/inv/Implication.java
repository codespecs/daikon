package daikon.inv;

import daikon.*;

import java.util.*;

import utilMDE.*;

public class Implication
  extends Invariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public Invariant predicate;
  public Invariant consequent;
  public boolean iff;

  protected Implication(PptSlice ppt) {
    super(ppt);
    throw new Error("Don't instantiate Implication this way.");
  }

  private Implication(PptSlice ppt, Invariant predicate, Invariant consequent, boolean iff) {
    super(ppt);
    Assert.assert(ppt instanceof PptSlice0);
    // Should these be true?
    // Assert.assert(predicate.ppt == ppt);
    // Assert.assert(consequent.ppt == ppt);
    this.predicate = predicate;
    this.consequent = consequent;
    this.iff = iff;
    ppt.invs.add(this);
    // System.out.println("Added implication invariant to " + ppt.name)
    // System.out.println("  " + this.format());
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
    if ((predicate.getClass() == consequent.getClass())
        && predicate.isSameFormula(consequent)) {
      return null;
    } 
    
    // Don't add this Implication to the program point if the program
    // point already has this implication.  This is slow and dumb; we
    // should use hashing for O(1) check instead.
    for (Iterator i = ppt.implication_view.invs.iterator(); i.hasNext(); ) {
      Implication existing = (Implication) i.next();
      if (existing.iff != iff) continue;
      if (existing.consequent.getClass() != consequent.getClass()) continue;
      if (existing.predicate.getClass() != predicate.getClass()) continue;
      // Why not instead check var_info indices plus isSameFormula?
      // Should use PptSlice identity check instead?
      if (! existing.consequent.format().equals(consequent.format())) continue;
      if (! existing.predicate.format().equals(predicate.format())) continue;
      return null;
    }
    
    return new Implication(ppt.implication_view, predicate, consequent, iff);
  }

  protected double computeProbability() {
    double pred_prob = predicate.computeProbability();
    double cons_prob = consequent.computeProbability();
    if ((pred_prob == PROBABILITY_NEVER)
        || (cons_prob == PROBABILITY_NEVER))
      return PROBABILITY_NEVER;
    return prob_and(pred_prob, cons_prob);
  }

  // We don't resurrect implications, right?
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  public String repr() {
    return "[Implication: " + predicate.repr()
      + " => " + consequent.repr() + "]";
  }

  public String format() {
    String arrow = (iff ? "  <==>  " : "  ==>  "); // "interned"
    return "(" + predicate.format() + ")" + arrow + "(" + consequent.format() + ")";
  }

    public String format_java() {
      String mid = (iff ? " == " : " || !");
      return "(" + consequent.format_java() + ")" + mid + "(" +predicate.format_java() + ")";
    }

  /* IOA */
  public String format_ioa() {
    String arrow = (iff ? "  <=>  " : "  =>  ");
    String pred_fmt = predicate.format_ioa();
    String consq_fmt = consequent.format_ioa();
    return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
  }


  public String format_esc() {
    String arrow = (iff ? "  ==  " : "  ==>  "); // "interned"
    String pred_fmt = predicate.format_esc();
    String consq_fmt = consequent.format_esc();
    return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
  }

  public String format_simplify() {
    String cmp = (iff ? "IFF" : "IMPLIES");
    String pred_fmt = predicate.format_simplify();
    String consq_fmt = consequent.format_simplify();
    return "(" + cmp + " " + pred_fmt + " " + consq_fmt + ")";
  }

  /// Completely confused ESC implementation; use better, briefer one.
  // private String make_impl(String pred, String cons) {
  //   return "(" + pred + ")  ==>  (" + cons + ")";
  // }
  // public String format_esc() {
  //   // Slightly gross to have this on one line instead of two separate ones
  //   String arrow = (iff ? "  <==>  " : "  ==>  "); // "interned"
  //   String pred = predicate.format_esc();
  //   String cons = consequent.format_esc();
  //   if (iff) {
  //     return "((" + make_impl(pred, cons) + ")   &&   ("
  //       + make_impl(cons, pred)
  //       + "))";
  //   } else {
  //     return make_impl(pred, cons);
  //   }
  // }

  public boolean isObviousDerived() {
    return consequent.isObviousDerived();
  }

  public boolean isObviousImplied() {
    return consequent.isObviousImplied();
  }

  public boolean isSameFormula(Invariant other) {
    return (predicate.isSameFormula(((Implication)other).predicate)
            && consequent.isSameFormula(((Implication)other).consequent));
  }

  /* [INCR]
  public boolean hasOnlyConstantVariables() {
    return predicate.hasOnlyConstantVariables();
  }
  */

  // An implication is only interesting if both the predicate and
  // consequent are interesting
  public boolean isInteresting() {
    return (predicate.isInteresting() && consequent.isInteresting());
  }

}
