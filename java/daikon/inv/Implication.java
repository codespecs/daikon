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
    return "(" + predicate.format() + ")" + arrow + "(" + consequent.format() + ")";
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

  public boolean isSameFormula(Invariant other) {
    return (predicate.isSameFormula(((Implication)other).predicate)
            && consequent.isSameFormula(((Implication)other).consequent));
  }

  public boolean hasOnlyConstantVariables() {
    return predicate.hasOnlyConstantVariables();
  }

}
