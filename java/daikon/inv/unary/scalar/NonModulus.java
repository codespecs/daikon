package daikon.inv.unary.scalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;
import java.util.*;

public class NonModulus extends SingleScalar {
  // Set elements = new HashSet();
  SortedSet elements = new TreeSet();

  private long modulus = 0;
  private long remainder = 0;
  // The next two variables indicate whether the "modulus" and "result"
  // fields are up to date.
  // Indicates that no nonmodulus has been found; maybe with more
  // samples, one will appear.
  private boolean no_result_yet = false;
  // We don't continuously keep the modulus and remainder field up to date.
  // This indicates whether it is.
  private boolean results_accurate = false;

  private NonModulus(PptSlice ppt) {
    super(ppt);
  }

  public static NonModulus instantiate(PptSlice ppt) {
    return new NonModulus(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "NonModulus(" + var().name + "): "
      + "m=" + modulus + ",r=" + remainder
      + "; probability = " + probability;
  }

  public String format() {
    return var().name + " != " + remainder + "  (mod " + modulus + ")";
  }


  // Set either modulus and remainder, or no_result_yet.
  void updateResults() {
    if (results_accurate)
      return;
    if (elements.size() == 0) {
      no_result_yet = true;
    } else {
      // Do I want to communicate back some information about the smallest
      // possible modulus?
      long[] result = MathMDE.nonmodulus_strict_long(elements.iterator());
      if (result == null) {
	no_result_yet = true;
      } else {
	remainder = result[0];
	modulus = result[1];
        no_result_yet = false;
      }
    }
    results_accurate = true;
  }

  public void add_modified(long value, int count) {
    if (elements.add(Intern.internedLong(value))
	&& results_accurate
	&& (MathMDE.mod_positive(value, modulus) == remainder))
      results_accurate = false;
  }

  protected double computeProbability() {
    updateResults();
    if (no_result_yet)
      return Invariant.PROBABILITY_UNKNOWN;
    double probability_one_elt_nonmodulus = 1 - 1.0/modulus;
    return Math.pow(probability_one_elt_nonmodulus, ppt.num_mod_non_missing_samples());
  }

  public boolean isSameFormula(Invariant o)
  {
    updateResults();
    if (no_result_yet)
      return false;

    NonModulus other = (NonModulus) o;
    other.updateResults();
    if (other.no_result_yet)
      return false;

    return
      (modulus == other.modulus) &&
      (remainder == other.remainder);
  }

  public boolean hasModulusRemainder(long modulus, long remainder) {
    updateResults();
    if (no_result_yet)
      return false;

    return ((modulus == this.modulus)
            && (remainder == this.remainder));
  }


  public boolean isExclusiveFormula(Invariant o) {
    updateResults();
    if (no_result_yet)
      return false;
    if (o instanceof NonModulus) {
      NonModulus other = (NonModulus) o;
      other.updateResults();
      if (other.no_result_yet)
        return false;
      return ((modulus == other.modulus)
              && (remainder != other.remainder));
    } else if (o instanceof Modulus) {
      Modulus other = (Modulus) o;
      return ((modulus == other.modulus)
              && (remainder == other.remainder));
    }

    return false;
  }

}
