package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;


// Similar to NonNull; if I change this, consider changing it, too.
// Actually, I should just abstract out the common code.

class NonZero extends SingleScalar {
  int min = Integer.MAX_VALUE;
  int max = Integer.MIN_VALUE;

  private NonZero(PptSlice ppt_) {
    super(ppt_);
  }

  public static NonZero instantiate(PptSlice ppt) {
    return new NonZero(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "NonZero(" + var().name + "): "
      + !no_invariant + ",min=" + min + ",max=" + max
      + "; probability = " + probability;
  }

  public String format() {
    if ((!no_invariant) && justified())
      return var().name + " != 0";
    else
      return null;
  }


  public void add_modified(int v, int count) {
    // The min and max tests will simultaneoulsy succeed exactly once (for
    // the first value).
    if (v == 0) {
      no_invariant = true;
      return;
    }
    if (v < min) min = v;
    if (v > max) max = v;
    // probability_cache_accurate = false;
  }

  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    // Maybe just use 0 as the min or max instead, and see what happens:
    // see whether the "nonzero" invariant holds anyway.  In that case,
    // do still check for no values yet received.
    else if ((min > 0) || (max < 0))
      return Invariant.PROBABILITY_UNKNOWN;
    else {
      int range = max - min + 1;
      double probability_one_elt_nonzero = 1 - 1.0/range;
      // This could underflow; so consider doing
      //   double log_confidence = self.samples*math.log(probability);
      // then calling Math.exp (if the value is in the range that wouldn't
      // cause underflow).
      return Math.pow(probability_one_elt_nonzero, ppt.num_mod_non_missing_samples());
    }
  }
}
