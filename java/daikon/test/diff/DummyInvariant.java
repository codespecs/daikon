// A dummy invariant used for testing purposes

package daikon.test.diff;

import daikon.*;
import daikon.inv.*;

public class DummyInvariant
  extends Invariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public String formula;
  public double probability;
  public boolean interesting;
  public boolean isWorthPrinting;

  public DummyInvariant(PptSlice ppt, String formula, boolean justified) {
    this(ppt, formula, (justified ? .001 : 1), true, true);
  }

  public DummyInvariant(PptSlice ppt, String formula,
                        boolean justified, boolean interesting) {
    this(ppt, formula, (justified ? .001 : 1), interesting, true);
  }


  public DummyInvariant(PptSlice ppt, String formula,
                        boolean justified, boolean interesting,
                        boolean isWorthPrinting) {
    this(ppt, formula, (justified ? .001 : 1), interesting, isWorthPrinting);
  }

  public DummyInvariant(PptSlice ppt, String formula, double probability) {
    this(ppt, formula, probability, true, true);
  }

  public DummyInvariant(PptSlice ppt, String formula,
                        double probability, boolean interesting) {
    this(ppt, formula, probability, interesting, true);
  }

  public DummyInvariant(PptSlice ppt, String formula,
                        double probability, boolean interesting,
                        boolean isWorthPrinting) {
    super(ppt);
    this.formula = formula;
    this.probability = probability;
    this.interesting = interesting;
    this.isWorthPrinting = isWorthPrinting;
  }

  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  public boolean justified() {
    return (probability <= .01);
  }

  public boolean isInteresting() {
    return interesting;
  }

  public boolean isSameInvariant(Invariant other) {
    return this.isSameFormula(other);
  }

  public boolean isSameFormula(Invariant other) {
    if (other instanceof DummyInvariant) {
      DummyInvariant o = (DummyInvariant) other;
      return this.formula.equals(o.formula);
    } else {
      return false;
    }
  }

  public double computeProbability() {
    return probability;
  }

  public String repr() {
    return "DummyInvariant(" + ppt.arity + "," + formula + "," + probability + ")";
  }

  public String format_using(OutputFormat format) {
    return repr();
  }

  // IsWorthPrinting should not be overridden by subclasses.
  // But this subclass is special:  it's not really an invariant,
  // but is only used for testing.
  public boolean isWorthPrinting() {
    return isWorthPrinting;
  }

}
