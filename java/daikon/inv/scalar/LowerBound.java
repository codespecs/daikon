package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;

// LowerBound >= 0 is implied by the variable being the length of an array or list.
//         self.nonnegative_obvious = (self.var_infos != None) and ("size(" == self.var_infos[0].name[0:5])
// That affects only printing, I think.


// very similar to UpperBound; don't change one without changing the other
class LowerBound extends SingleScalar {

  // min1 < min2 < min3
  int min1 = Integer.MAX_VALUE;
  int num_min1 = 0;
  int mod_min1 = 0;
  int min2 = Integer.MAX_VALUE;
  int num_min2 = 0;
  int mod_min2 = 0;
  int min3 = Integer.MAX_VALUE;
  int num_min3 = 0;
  int mod_min3 = 0;
  int max = Integer.MIN_VALUE;

  LowerBound(PptSlice ppt_) {
    super(ppt_);
  }

//   LowerBound(Ppt ppt_, VarInfo var_info_) {
//     super(ppt_, var_info_);
//   }

  public String repr() {
    double probability = getProbability();
    return "LowerBound(" + var().name + "): "
      + min1 + "; probability = " + probability;
  }

  public String format() {
    if (justified())
      return var().name + " >= " + min1;
    else
      return null;
  }

  public void add(int value, int modified, int count) {
    probability_cache_accurate = false;

    int mod_count = (modified == ValueTuple.MODIFIED) ? count : 0;
    int v = value;

    if (v > max) max = v;

    if (v == min1) {
      num_min1 += count;
      mod_min1 += mod_count;
    } else if (v < min1) {
      min3 = min2;
      num_min3 = num_min2;
      mod_min3 = mod_min3;
      min2 = min1;
      num_min2 = num_min1;
      mod_min2 = mod_min1;
      min1 = v;
      num_min1 = count;
      mod_min1 = mod_count;
    } else if (v == min2) {
      num_min2 += count;
      mod_min2 += mod_count;
    } else if (v < min2) {
      min3 = min2;
      num_min3 = num_min2;
      min2 = v;
      num_min2 = count;
    } else if (v == min3) {
      num_min3 += count;
      mod_min3 += mod_count;
    } else if (v < min3) {
      min3 = v;
      num_min3 = count;
    }
  }

  public void add_modified(int value, int count) {
    add(value, ValueTuple.MODIFIED, count);
  }

  protected double computeProbability() {
    int values = ppt.num_values();
    if (values < 3)
      return Invariant.PROBABILITY_UNKNOWN;
    if (num_min1 < 3)
      return Invariant.PROBABILITY_UNKNOWN;

    // Accept a bound if:
    //  * it contains more than twice as many elements as it ought to by
    //    chance alone, and that number is at least 3.
    //  * it and its predecessor/successor both contain more than half
    //    as many elements as they ought to by chance alone, and at
    //    least 3.

    double avg_samples_per_val = ppt.num_mod_non_missing_samples() / values;

    System.out.println("Fix computation of LowerBound.computeProbability()");
    boolean truncated_justified = num_min1 > 2*avg_samples_per_val;
    boolean uniform_justified = ((num_min1 > avg_samples_per_val/2)
			      && (num_min2 > avg_samples_per_val/2)
			      && (num_min3 > avg_samples_per_val/2));
    if (truncated_justified || uniform_justified)
      return 0;
    else
      return 1;
  }
}
