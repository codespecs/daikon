package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;


// similar to NonNull; if I change this, consider changing it, too
class NonZero extends SingleScalar {
  int min = Integer.MAX_VALUE;
  int max = Integer.MIN_VALUE;

  boolean can_be_zero = false;

  public NonZero(PptSlice ppt_) {
    super(ppt_);
  }

//   NonZero(Ppt ppt_, VarInfo var_info_) {
//     super(ppt_, var_info_);
//   }

  public String repr() {
    double probability = getProbability();
    return "NonZero(" + var().name + "): "
      + !can_be_zero + ",min=" + min + ",max=" + max
      + "; probability = " + probability;
  }

  public String format() {
    if ((!can_be_zero) && justified())
      return var().name + " != 0";
    else
      return null;
  }


  public void add_modified(int v, int count) {
    if (can_be_zero)
      return;
    // The min and max tests will simultaneoulsy succeed exactly once (for
    // the first value).
    if (v < min) min = v;
    if (v > max) max = v;
    if (v == 0) can_be_zero = true;
    probability_cache_accurate = false;
  }

  protected double computeProbability() {
    if (can_be_zero)
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
