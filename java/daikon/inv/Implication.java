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
    Assert.assertTrue(ppt instanceof PptSlice0);
    // Should these be true?
    // Assert.assertTrue(predicate.ppt == ppt);
    // Assert.assertTrue(consequent.ppt == ppt);
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
    if (predicate.isSameInvariant(consequent)) {
      return null;
    }
    //eliminate some "uninteresting" implications, like OneOf predicates and
    //consequents, which are usually not interesting.
    // JWN adds: Why not use the isInteresting method?  Is it Because
    // you still want Bound invariants?
    if (predicate instanceof OneOf) {
      if ( ((OneOf) predicate).num_elts() > 1)
        return null;
    }
    if (consequent instanceof OneOf) {
      if ( ((OneOf) consequent).num_elts() > 1)
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

    Implication result = new Implication(ppt.implication_view, predicate, consequent, iff);
    return result;
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

  public String format_using(OutputFormat format) {
    String pred_fmt = predicate.format_using(format);
    String consq_fmt = consequent.format_using(format);
    if (format == OutputFormat.DAIKON) {
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
    } else {
      return format_unimplemented(format);
    }
  }

  public boolean isValidEscExpression() {
    return predicate.isValidEscExpression()
      && consequent.isValidEscExpression();
  }

  public boolean isObviousDerived() {
    return consequent.isObviousDerived();
  }

  public boolean isObviousImplied() {
    return consequent.isObviousImplied();
  }

  public boolean isSameFormula(Invariant other) {
    Implication other_implic = (Implication)other;
    // Guards are necessary because the contract of isSameFormula states
    // that the argument is of the same class as the receiver.
    return (((predicate.getClass() == other_implic.predicate.getClass())
            && predicate.isSameFormula(other_implic.predicate))
            && ((consequent.getClass() == other_implic.consequent.getClass())
            && consequent.isSameFormula(other_implic.consequent)));
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
