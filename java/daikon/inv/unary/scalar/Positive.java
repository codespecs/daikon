package daikon.inv.unary.scalar;

import daikon.*;

// This invariant is true if the variable is always positive (greater than 0).
// This invariant is provided for pedagogical reasons only -- in reality,

public class Positive extends SingleScalar {

  ///
  /// Required methods
  ///

  private Positive(PptSlice ppt) {
    super(ppt);
  }

  public static Positive instantiate(PptSlice ppt) {
    return new Positive(ppt);
  }

  // A printed representation for user output
  public String format() {
    return var().name.name() + " > 0";
  }

  // This is called when a new sample is encountered
  public void add_modified(long v, int count) {
    if (v <= 0) {
      flowThis();
      destroy();
      return;
    }
  }

  protected double computeProbability() {
    // Assume that every variable has a .5 chance of being positive by
    // chance.  Then a set of n values have a have (.5)^n chance of all
    // being positive by chance.
    return Math.pow(.5, ppt.num_mod_non_missing_samples());
  }

}
