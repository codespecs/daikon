package daikon.inv;

import daikon.*;

import java.util.*;

import utilMDE.*;

public class Implication extends Invariant {

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
    this.predicate = predicate;
    this.consequent = consequent;
    this.iff = iff;
    ppt.invs.add(this);
    // System.out.println("Added implication invariant to " + ppt.name
    //                    + "\n  " + this.format());
  }

  static public Implication makeImplication(PptTopLevel ppt, Invariant predicate, Invariant consequent, boolean iff) {
    if ((predicate.getClass() == consequent.getClass())
        && predicate.isSameFormula(consequent)) {
      return null;
    }
    return new Implication(ppt.implication_view, predicate, consequent, iff);
  }

  protected double computeProbability() {
    double pred_prob = predicate.computeProbability();
    double cons_prob = consequent.computeProbability();
    if ((pred_prob > PROBABILITY_UNJUSTIFIED)
        || (cons_prob > PROBABILITY_UNJUSTIFIED))
      return Math.max(pred_prob, cons_prob);
    return 1-(1-pred_prob)*(1-cons_prob);
  }

  public String repr() {
    return "[Implication: " + predicate.repr()
      + " => " + consequent.repr() + "]";
  }

  public String format() {
    String arrow = (iff ? "  <==>  " : "  ==>  "); // "interned"
    return "(" + predicate.format() + ")" + arrow + consequent.format();
  }

  public String format_esc() {
    String arrow = (iff ? "  <==>  " : "  ==>  "); // "interned"
    return "(" + predicate.format_esc() + ")" + arrow + "(" + consequent.format_esc() + ")";
  }

  public boolean isSameFormula(Invariant other) {
    return (predicate.isSameFormula(((Implication)other).predicate)
            && consequent.isSameFormula(((Implication)other).consequent));
  }

  public boolean hasOnlyConstantVariables() {
    return predicate.hasOnlyConstantVariables();
  }

}
