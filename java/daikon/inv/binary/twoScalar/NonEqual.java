package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;


// Also serves as NonAliased.
public final class NonEqual extends TwoScalar {
  long min1 = Long.MAX_VALUE;
  long min2 = Long.MAX_VALUE;
  long max1 = Long.MIN_VALUE;
  long max2 = Long.MIN_VALUE;

  // If nonzero, use this as the range instead of the actual range.
  // This lets one use a specified probability of nonzero (say, 1/10
  // for pointers).
  long override_range = 0;

  // Get this from the Ppt
  // int samples = 0;

  protected NonEqual(PptSlice ppt) {
    super(ppt);
  }

  public static NonEqual instantiate(PptSlice ppt) {
    NonEqual result = new NonEqual(ppt);
    if (! ppt.var_infos[0].type.isIntegral()) {
      result.override_range = 10;
    }
    // System.out.println("Nonequal override_range = " + result.override_range + " for " + ppt.name);
    return result;
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
    return var1().name + " != " + var2().name;
  }


  public void add_modified(long v1, long v2, int count) {
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
      double probability_one_nonequal;
      if (override_range != 0) {
        probability_one_nonequal = 1 - 1/(double)override_range;
      } else {
        long overlap = Math.min(max1, max2) - Math.max(min1, min2);
        // Looks like we're comparing pointers.  Fix this later.
        if (overlap < 0)
          return Invariant.PROBABILITY_JUSTIFIED;

        Assert.assert(overlap >= 0);
        overlap++;
        long range1 = max1 - min1 + 1;
        long range2 = max2 - min2 + 1;

        // probability of being equal by chance
        //  = (overlap/range1) * (overlap/range2) * (1/overlap)
        //  = overlap/(range1 * range2)

        probability_one_nonequal = 1-((double)overlap)/(range1 * range2);
      }

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

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof NonEqual);
    return true;
  }
  
}
