package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;


// very similar to LowerBound; don't change one without changing the other
class UpperBound extends SingleScalar {

  int max1 = Integer.MIN_VALUE;
  int num_max1 = 0;
  int mod_max1 = 0;
  int max2 = Integer.MIN_VALUE;
  int num_max2 = 0;
  int mod_max2 = 0;
  int max3 = Integer.MIN_VALUE;
  int num_max3 = 0;
  int mod_max3 = 0;
  int min = Integer.MAX_VALUE;

  public UpperBound(PptSlice ppt_) {
    super(ppt_);
  }

//   public UpperBound(Ppt ppt_, VarInfo var_info_) {
//     super(ppt_, var_info_);
//   }

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

  public void add(int value, int modified, int count) {
    probability_cache_accurate = false;

    int mod_count = (modified == ValueTuple.MODIFIED) ? count : 0;
    int v = value;
    if (v < min) min = v;

    if (v == max1) {
      num_max1 += count;
      mod_max1 += mod_count;
    } else if (v > max1) {
      max3 = max2;
      num_max3 = num_max2;
      mod_max3 = mod_max3;
      max2 = max1;
      num_max2 = num_max1;
      mod_max2 = mod_max1;
      max1 = v;
      num_max1 = count;
      mod_max1 = mod_count;
    } else if (v == max2) {
      num_max2 += count;
      mod_max2 += mod_count;
    } else if (v > max2) {
      max3 = max2;
      num_max3 = num_max2;
      max2 = v;
      num_max2 = count;
    } else if (v == max3) {
      num_max3 += count;
      mod_max3 += mod_count;
    } else if (v > max3) {
      max3 = v;
      num_max3 = count;
    }
  }

  public void add_modified(int value, int count) {
    add(value, ValueTuple.MODIFIED, count);
  }

  protected double computeProbability() {
    int values = ppt.num_values();
    if (values < 3)
      return Invariant.PROBABILITY_UNKNOWN;
    if (num_max1 < 3)
      return Invariant.PROBABILITY_UNKNOWN;

    // Accept a bound if:
    //  * it contains more than twice as many elements as it ought to by
    //    chance alone, and that number is at least 3.
    //  * it and its predecessor/successor both contain more than half
    //    as many elements as they ought to by chance alone, and at
    //    least 3.

    double avg_samples_per_val = ppt.num_mod_non_missing_samples() / values;

    // System.out.println("  [Need to fix computation of UpperBound.computeProbability()]");
    boolean truncated_justified = num_max1 > 2*avg_samples_per_val;
    boolean uniform_justified = ((num_max1 > avg_samples_per_val/2)
                                 && (num_max2 > avg_samples_per_val/2)
                                 && (num_max3 > avg_samples_per_val/2));
    if (truncated_justified || uniform_justified)
      return 0;
    else
      return 1;
  }
}
