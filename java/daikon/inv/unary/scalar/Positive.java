package daikon.inv.unary.scalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.DiscardCode;
import daikon.inv.InvariantStatus;

// This invariant is true if the variable is always positive (greater than 0).
// This invariant is provided for pedagogical reasons only.

/**
 * Represents the invariant 'x > 0' where x is a long scalar.  This exists
 * only as an example for the purposes of the manual.  It isn't actually
 * used (it is replaced by the more general invariant LowerBound).
 **/

public class Positive
  extends SingleScalar
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

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
  public String format_using(OutputFormat format) {
    return var().name.name() + " > 0";
  }

  // This is called when a new sample is encountered
  public InvariantStatus add_modified(long v, int count) {
    if (v <= 0) {
      return InvariantStatus.FALSIFIED;
    }
    return InvariantStatus.NO_CHANGE;
  }

  protected double computeConfidence() {
    // Assume that every variable has a .5 chance of being positive by
    // chance.  Then a set of n values have a have (.5)^n chance of all
    // being positive by chance.
    return 1 - Math.pow(.5, ppt.num_mod_samples());
  }

  protected double computeProbability() {
    // Assume that every variable has a .5 chance of being positive by
    // chance.  Then a set of n values have a have (.5)^n chance of all
    // being positive by chance.
    return Math.pow(.5, ppt.num_mod_samples());
  }

}
