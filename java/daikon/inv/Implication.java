package daikon.inv;

import daikon.*;

import java.util.*;

import utilMDE.*;

public class Implication extends Invariant {

  Invariant predicate;
  Invariant consequent;

  protected Implication(PptSlice ppt) {
    super(ppt);
    throw new Error("Don't instantiate Implication this way.");
  }

  public Implication(PptSlice ppt, Invariant predicate, Invariant consequent) {
    super(ppt);
    Assert.assert(ppt instanceof PptSlice0);
    this.predicate = predicate;
    this.consequent = consequent;
    ppt.invs.add(this);
    // System.out.println("Added implication invariant to " + ppt.name
    //                    + "\n  " + this.format());
  }

  public Implication(PptTopLevel ppt, Invariant predicate, Invariant consequent) {
    this(ppt.implication_view, predicate, consequent);
  }


  protected double computeProbability() {
    return Math.min(predicate.computeProbability(),
                    consequent.computeProbability());
  }

  public String repr() {
    return "[Implication: " + predicate.repr()
      + " => " + consequent.repr() + "]";
  }

  public String format() {
    return predicate.format() + " => " + consequent.format();
  }

  public boolean isSameFormula(Invariant other) {
    return (predicate.isSameFormula(((Implication)other).predicate)
            && consequent.isSameFormula(((Implication)other).consequent));
  }

}
