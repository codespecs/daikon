package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

class Modulus extends SingleScalar {

  long modulus = 0;
  long remainder = 0;

  // An arbitrarily-chosen value used for computing the differences among
  // all the values.  Arbitrary initial value 2222 will be replaced by the
  // first actual value seen.
  long value1 = 2222;
  // used for initializing value1
  boolean no_values_seen = true;

  private Modulus(PptSlice ppt) {
    super(ppt);
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

  public void add_modified(long value, int count) {
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
      long new_modulus_long = MathMDE.gcd(modulus, value1 - value);
      int new_modulus;
      if (new_modulus_long > Integer.MAX_VALUE
          || (new_modulus_long < Integer.MIN_VALUE)) {
        new_modulus = 1;
      } else {
        new_modulus = (int) new_modulus_long;
      }
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
