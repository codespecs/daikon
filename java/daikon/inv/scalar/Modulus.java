package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

class Modulus extends SingleScalar {

  int modulus = 0;
  int remainder = 0;

  // An arbitrarily-chosen value used for computing the differences among
  // all the values.  Arbitrary initial value 2222 will be replaced by the
  // first actual value seen.
  int value1 = 2222;
  // used for initializing value1
  boolean no_values_seen = true;

  private Modulus(PptSlice ppt_) {
    super(ppt_);
  }

  public static Modulus instantiate(PptSlice ppt) {
    return new Modulus(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "Modulus(" + var().name + "): "
      + "m=" + modulus + ",r=" + remainder
      + "; probability = " + probability;
  }

  public String format() {
    if (justified())
      return var().name + " = " + remainder + "  (mod " + modulus + ")";
    else
      return null;
  }

  public void add_modified(int value, int count) {
    if (modulus == 1) {
      // We shouldn't ever get to this case; the invariant should have been
      // destroyed instead.
      throw new Error("Modulus = 1");
      // Assert.assert(no_invariant);
      // // We already know this probability fails
      // return;
    } else if (no_values_seen) {
      value1 = value;
      no_values_seen = false;
      return;
    } else if (value == value1) {
      // no new information, so nothing to do
      return;
    } else if (modulus == 0) {
      // only one value seen so far
      modulus = value1 - value;
      if (modulus == 1) {
        destroy();
        return;
      }
      remainder = MathMDE.mod_positive(value, modulus);
    } else {
      int new_modulus = MathMDE.gcd(modulus, value1 - value);
      if (new_modulus != modulus) {
        if (new_modulus == 1) {
          destroy();
          return;
        } else {
          remainder = remainder % new_modulus;
          modulus = new_modulus;
        }
      }
      // probability_cache_accurate = false;
    }
    Assert.assert(modulus != 1);
  }

  protected double computeProbability() {
    if (modulus == 1)
      return Invariant.PROBABILITY_NEVER;
    if (modulus == 0)
      return Invariant.PROBABILITY_UNKNOWN;
    double probability_one_elt_modulus = 1 - 1.0/modulus;
    return Math.pow(probability_one_elt_modulus, ppt.num_mod_non_missing_samples());
  }
}
