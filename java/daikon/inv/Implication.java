package daikon.inv;

import daikon.*;

import java.util.*;

import utilMDE.*;

// Here Implication is reimplemented as an extension of the new general
// Joiner class

public class Implication
  extends Joiner
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020722L;

  public boolean iff;

  protected Implication(PptSlice ppt, Invariant predicate, Invariant consequent, boolean iff) {
    super(ppt, predicate, consequent);
    this.iff = iff;
  }

  public Invariant predicate() { return left; }
  public Invariant consequent() { return right; }

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
      return null;
    }

    // System.out.println("Adding implication: ");
    // System.out.println("Predicate: " + predicate.format_using(OutputFormat.JML));
    // System.out.println("Consequent: " + consequent.format_using(OutputFormat.JML));
    Implication result = new Implication(ppt.joiner_view, predicate, consequent, iff);
    return result;
  }

  protected double computeProbability() {
    double pred_prob = left.computeProbability();
    double cons_prob = right.computeProbability();
    if ((pred_prob == PROBABILITY_NEVER)
        || (cons_prob == PROBABILITY_NEVER))
      return PROBABILITY_NEVER;
    return prob_and(pred_prob, cons_prob);
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
    } else {
      return format_unimplemented(format);
    }
  }

  public boolean isObviousDerived() {
    return right.isObviousDerived();
  }

  public boolean isObviousImplied() {
    return right.isObviousImplied();
  }

  /* [INCR]
  public boolean hasOnlyConstantVariables() {
    return predicate.hasOnlyConstantVariables();
  }
  */
}
