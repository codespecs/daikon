package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;


// very similar to LowerBound; don't change one without changing the other
class UpperBound extends SingleScalar {

  // max1 > max2 > max3
  int max1 = Integer.MIN_VALUE;
  int num_max1 = 0;
  int max2 = Integer.MIN_VALUE;
  int num_max2 = 0;
  int max3 = Integer.MIN_VALUE;
  int num_max3 = 0;
  int min = Integer.MAX_VALUE;

  private UpperBound(PptSlice ppt_) {
    super(ppt_);
  }

  public static UpperBound instantiate(PptSlice ppt) {
    return new UpperBound(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "UpperBound(" + var().name + "): "
      + max1 + "; probability = " + probability;
  }

  public String format() {
    if (justified())
      return var().name + " <= " + max1;
    else
      return null;
  }


  public void add_modified(int value, int count) {
    // probability_cache_accurate = false;

    int v = value;
    if (v < min) min = v;

    if (v == max1) {
      num_max1 += count;
    } else if (v > max1) {
      max3 = max2;
      num_max3 = num_max2;
      max2 = max1;
      num_max2 = num_max1;
      max1 = v;
      num_max1 = count;
    } else if (v == max2) {
      num_max2 += count;
    } else if (v > max2) {
      max3 = max2;
      num_max3 = num_max2;
      max2 = v;
      num_max2 = count;
    } else if (v == max3) {
      num_max3 += count;
    } else if (v > max3) {
      max3 = v;
      num_max3 = count;
    }
  }

  protected double computeProbability() {
    int values = ppt.num_values();
    if (values < 3)
      return Invariant.PROBABILITY_UNKNOWN;
    // This used to count number of samples instead of number of modified
    // samples.
    if (num_max1 < 3)
      return Invariant.PROBABILITY_UNKNOWN;

    // Accept a bound if:
    //  * it contains more than twice as many elements as it ought to by
    //    chance alone, and that number is at least 3.
    //  * it and its predecessor/successor both contain more than half
    //    as many elements as they ought to by chance alone, and at
    //    least 3.

    int range = max1 - min + 1;
    double avg_samples_per_val = ((double) ppt.num_mod_non_missing_samples()) / range;

    // System.out.println("  [Need to fix computation of UpperBound.computeProbability()]");
    boolean truncated_justified = num_max1 > 5*avg_samples_per_val;
    // To do:  should permit min1, min2, min3 to be non-consecutive
    // only if there is a modulus invariant over the variable.
    boolean uniform_justified = (((max1 - max2) == (max2 - max3))
                                 && (num_max1 > avg_samples_per_val/2)
                                 && (num_max2 > avg_samples_per_val/2)
                                 && (num_max3 > avg_samples_per_val/2));
    if (truncated_justified || uniform_justified)
      return 0;
    else
      return 1;
  }
}
