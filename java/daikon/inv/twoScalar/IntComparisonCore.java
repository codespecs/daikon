package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;


// Also see NonEqual, NonAliased
public class IntComparisonCore {

  public boolean can_be_eq = false;
  public boolean can_be_lt = false;
  public boolean can_be_gt = false;

  // If set, these mean that the invariant is obvious as soon as the
  // specified flag is set.  For instance, we might set obvious_can_be_lt
  // for "b[0] cmp max(b)".  It would be interesting and relevant if we
  // found those quantities equal, but if the relationship can be "<", then
  // it is "<=" (because we know they can be "="), so it's obvious and
  // uninteresting.

  // These are final for efficiency's sake.
  public final boolean only_check_eq;
  public final boolean obvious_can_be_lt;
  public final boolean obvious_can_be_gt;
  public final boolean obvious_can_be_le;
  public final boolean obvious_can_be_ge;

  Invariant wrapper;

  public IntComparisonCore(Invariant wrapper_) {
    this(wrapper_, false, false, false, false);
  }

// , boolean only_eq
  public IntComparisonCore(Invariant wrapper_, boolean obvious_lt, boolean obvious_gt, boolean obvious_le, boolean obvious_ge) {
    wrapper = wrapper_;
//    only_check_eq = only_eq;
only_check_eq = false;
    obvious_can_be_lt = obvious_lt;
    obvious_can_be_gt = obvious_gt;
    obvious_can_be_le = obvious_le;
    obvious_can_be_ge = obvious_ge;
  }

  public void add_modified(int v1, int v2, int count) {
    if (v1 == v2)
      can_be_eq = true;
    else if (v1 < v2)
      can_be_lt = true;
    else
      can_be_gt = true;
    if ((can_be_lt && can_be_gt)
        || (only_check_eq && (can_be_lt || can_be_gt))
        || (obvious_can_be_lt && can_be_lt)
        || (obvious_can_be_gt && can_be_gt)
        || (obvious_can_be_le && can_be_lt && can_be_eq)
        || (obvious_can_be_ge && can_be_gt && can_be_eq)) {
      wrapper.destroy();
      return;
    }
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
