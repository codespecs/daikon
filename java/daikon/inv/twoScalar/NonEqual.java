package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;


// Similar to NonAliased; if I change this, consider changing it, too.
// Better, abstract out their common parts.
public class NonEqual extends TwoScalar {
  int min1 = Integer.MAX_VALUE;
  int min2 = Integer.MAX_VALUE;
  int max1 = Integer.MIN_VALUE;
  int max2 = Integer.MIN_VALUE;

  // Get this from the Ppt
  // int samples = 0;

  protected NonEqual(PptSlice ppt_) {
    super(ppt_);
  }

  public static NonEqual instantiate(PptSlice ppt) {
    return new NonEqual(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "NonEqual" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + ",min1=" + min1
      + ",min2=" + min2
      + ",max1=" + max1
      + ",max2=" + max2
      + "; probability = " + probability;
  }

  public String format() {
    if ((!no_invariant) && justified()) {
      return var1().name + " != " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int v1, int v2, int count) {
    // probability_cache_accurate = false;
    if (v1 == v2) {
      destroy();
      return;
    }
    if (v1 < min1) min1 = v1;
    if (v1 > max1) max1 = v1;
    if (v2 < min2) min2 = v2;
    if (v2 > max2) max2 = v2;
  }



  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    else if ((min1 > max2) || (max1 < min2))
      return Invariant.PROBABILITY_UNKNOWN;
    else {
      int overlap = Math.min(max1, max2) - Math.max(min1, min2);
      // Looks like we're comparing pointers.  Fix this later.
      if (overlap < 0)
        return 0;

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

  // We don't suppress before creation because this uses different
  // justification rules than IntComparison.
  public boolean isObviousImplied() {
    IntComparison ic = IntComparison.find(ppt);
    if ((ic != null) && (! ic.core.can_be_eq) && ic.justified())
      return true;
    return false;
  }

}
