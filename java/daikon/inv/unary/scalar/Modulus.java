package daikon.inv.unary.scalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

public class Modulus extends SingleScalar {

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
    return "Modulus" + varNames() + ": "
      + "modulus=" + modulus + ",remainder=" + remainder;
  }

  public String format() {
    return var().name + " == " + remainder + "  (mod " + modulus + ")";
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
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
      modulus = Math.abs(value1 - value);
      if (modulus == 1) {
        destroy();
        return;
      }
      remainder = MathMDE.mod_positive(value, modulus);
    } else {
      long new_modulus_long = Math.abs(MathMDE.gcd(modulus, value1 - value));
      int new_modulus;
      if (new_modulus_long > Integer.MAX_VALUE
          || (new_modulus_long < Integer.MIN_VALUE)) {
        new_modulus = 1;
      } else {
        new_modulus = (int) new_modulus_long;
        Assert.assert(new_modulus > 0);
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

  public boolean isSameFormula(Invariant other)
  {
    Modulus otherModulus = (Modulus) other;

    boolean thisMeaningless = (modulus == 0 || modulus == 1);
    boolean otherMeaningless = (otherModulus.modulus == 0 ||
                                otherModulus.modulus == 1);
    
    if (thisMeaningless && otherMeaningless) {
      return true;
    } else {
      return
        (modulus != 1) &&
        (modulus != 0) &&
        (modulus == otherModulus.modulus) &&
        (remainder == otherModulus.remainder);
    }
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if ((modulus == 0) || (modulus == 1))
      return false;

    // Weak test, can be strengthened.
    //  * x = 1 mod 4  is exclusive with  x = 6 mod 8
    //  * x = 1 mod 4  is exclusive with  x = 0 mod 2
    //  * x = 0 mod 4  is exclusive with  1 <= x <= 3
    if (other instanceof Modulus) {
      return ((modulus == ((Modulus) other).modulus)
              && (remainder != ((Modulus) other).remainder));
    } else if (other instanceof NonModulus) {
      return ((NonModulus) other).hasModulusRemainder(modulus, remainder);
    }

    return false;
  }

}
