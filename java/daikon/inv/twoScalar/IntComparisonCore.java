package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;


// Also see NonEqual, NonAliased
public class IntComparisonCore {

  final static boolean debugIntComparisonCore = false;

  public boolean can_be_eq = false;
  public boolean can_be_lt = false;
  public boolean can_be_gt = false;

  Invariant wrapper;

  public IntComparisonCore(Invariant wrapper_) {
    wrapper = wrapper_;
  }

  public void add_modified(int v1, int v2, int count) {
    if (v1 == v2)
      can_be_eq = true;
    else if (v1 < v2)
      can_be_lt = true;
    else
      can_be_gt = true;
    // Could set no_invariant here.
    wrapper.no_invariant = (can_be_lt && can_be_gt);
  }

  public double computeProbability() {
    if (wrapper.no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      // I don't know how to compute a better probability for this.
      return 0;
    }
  }

}



/// "Obvious" comparisons

// self.comparison_obvious = None
// # These variables are set to the name of the sequence, or None.
// # Avoid regular expressions wherever possible.
// min1 = (var1[0:4] == "min(") and var1[4:-1]
// max1 = (var1[0:4] == "max(") and var1[4:-1]
// # I think "find" does a regexp operation, unfortunately
// aref1 = string.find(var1, "[")
// if aref1 == -1:
//     aref1 = None
// else:
//     aref1 = var1[0:aref1]
// if min1 or max1 or aref1:
//     min2 = (var2[0:4] == "min(") and var2[4:-1]
//     max2 = (var2[0:4] == "max(") and var2[4:-1]
//     aref2 = string.find(var2, "[")
//     if aref2 == -1:
//         aref2 = None
//     else:
//         aref2 = var2[0:aref2]
//     if min1 and max2 and min1 == max2:
//         self.comparison_obvious = "<="
//     elif min1 and aref2 and min1 == aref2:
//         self.comparison_obvious = "<="
//     elif max1 and min2 and max1 == min2:
//         self.comparison_obvious = ">="
//     elif max1 and aref2 and max1 == aref2:
//         self.comparison_obvious = ">="
//     elif aref1 and min2 and aref1 == min2:
//         self.comparison_obvious = ">="
//     elif aref1 and max2 and aref1 == max2:
//         self.comparison_obvious = "<="
