package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;
import java.util.*;

class NonModulus extends SingleScalar {
  // Set elements = new HashSet();
  SortedSet elements = new TreeSet(new IntegerComparator());

  class IntegerComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      return ((Integer)o1).intValue() - ((Integer)o2).intValue();
    }
  }

  private int modulus = 0;
  private int remainder = 0;
  private boolean no_result = false;
  private boolean results_accurate = false;

  public NonModulus(PptSlice ppt_) {
    super(ppt_);
  }

// Old version
//   public NonModulus(Ppt ppt_, VarInfo var_info_) {
//     super(ppt_, var_info_);
//   }

  public String repr() {
    double probability = getProbability();
    return "NonModulus(" + var().name + "): "
      + "m=" + modulus + ",r=" + remainder
      + "; probability = " + probability;
  }

  public String format() {
    if (justified())
      return var().name + " != " + remainder + "  (mod " + modulus + ")";
    else
      return null;
  }


  void updateResults() {
    if (results_accurate)
      return;
    if (elements.size() == 0) {
      no_result = true;
    } else {
      int[] result = MathMDE.nonmodulus_strict(elements.iterator());
      if (result == null) {
	no_result = true;
      } else {
	remainder = result[0];
	modulus = result[1];
      }
    }
    results_accurate = true;
  }

  public void add_modified(int value, int count) {
    if (elements.add(new Integer(value))
	&& results_accurate
	&& (MathMDE.mod_positive(value, modulus) == remainder))
      results_accurate = false;
  }

  protected double computeProbability() {
    updateResults();
    if (no_result)
      return Invariant.PROBABILITY_UNKNOWN;
    double probability_one_elt_nonmodulus = 1 - 1.0/modulus;
    return Math.pow(probability_one_elt_nonmodulus, ppt.num_mod_non_missing_samples());
  }

}
