// A dummy invariant used for testing purposes

package daikon.test.diff;

import daikon.*;
import daikon.inv.*;

public class DummyInvariant extends Invariant {

  public String formula;
  public boolean justified;
  public boolean interesting;
  public boolean isWorthPrinting;

  public DummyInvariant(PptSlice ppt, String formula, boolean justified) {
    this(ppt, formula, justified, true, true);
  }

  public DummyInvariant(PptSlice ppt, String formula,
                        boolean justified, boolean interesting) {
    this(ppt, formula, justified, interesting, true);
  }


  public DummyInvariant(PptSlice ppt, String formula,
                        boolean justified, boolean interesting,
                        boolean isWorthPrinting) {
    super(ppt);
    this.formula = formula;
    this.justified = justified;
    this.interesting = interesting;
    this.isWorthPrinting = isWorthPrinting;
  }

  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  public boolean justified() {
    return justified;
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
    return 0;
  }

  public String repr() {
    return "DummyInvariant(" + ppt.arity + "," + formula + "," + justified + ")";
  }

  public String format() {
    return repr();
  }

  public String format_ioa(String a) {
    return repr();
  }

  public String format_esc() {
    return repr();
  }

  public String format_simplify() {
    return repr();
  }

  public boolean isWorthPrinting() {
    return isWorthPrinting;
  }

}
