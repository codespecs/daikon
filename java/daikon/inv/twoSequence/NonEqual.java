package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;


// Similar to NonAliased; if I change this, consider changing it, too.
public class NonEqual extends TwoSequence {
  int min1 = Integer.MAX_VALUE;
  int min2 = Integer.MAX_VALUE;
  int max1 = Integer.MIN_VALUE;
  int max2 = Integer.MIN_VALUE;

  boolean can_be_equal = false;

  // Get this from the Ppt
  // int samples = 0;

  NonEqual(PptSlice ppt_) {
    super(ppt_);
  }

//   NonEqual(Ppt ppt_, VarInfo var_info1_, VarInfo var_info2_) {
//     super(ppt_, var_info1_, var_info2_);
//   }

  public String repr() {
    double probability = getProbability();
    return "NonEqual" + varNames() + ": "
      + "can_be_equal=" + can_be_equal
      + ",min1=" + min1
      + ",min2=" + min2
      + ",max1=" + max1
      + ",max2=" + max2
      + "; probability = " + probability;
  }

  public String format() {
    if ((!can_be_equal) && justified()) {
      return var1().name + " != " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int v1, int v2, int count) {
    if (can_be_equal)
      return;
    probability_cache_accurate = false;
    if (v1 == v2) {
      can_be_equal = true;
      return;
    }
    if (v1 < min1) min1 = v1;
    if (v1 > max1) max1 = v1;
    if (v2 < min2) min2 = v2;
    if (v2 > max2) max2 = v2;
  }



  protected double computeProbability() {
    if (can_be_equal)
      return Invariant.PROBABILITY_NEVER;
    else if ((min1 > max2) || (max1 < min2))
      return Invariant.PROBABILITY_UNKNOWN;
    else {
      int overlap = Math.min(max1, max2) - Math.max(min1, min2);
      Assert.assert(overlap >= 0);
      overlap++;
      int range1 = max1 - min1 + 1;
      int range2 = max2 - min2 + 1;

      // probability of being equal by chance
      //  = (overlap/range1) * (overlap/range2) * (1/overlap)
      //  = overlap/(range1 * range2)

      double probability_one_nonequal = 1-((double)overlap)/(range1 * range2);

      return Math.pow(probability_one_nonequal, ppt.num_mod_non_missing_samples());
    }
  }
}

