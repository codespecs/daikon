package daikon.inv.binary.twoString;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import utilMDE.*;
import java.io.Serializable;

public final class StringComparisonCore
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  public boolean can_be_eq = false;
  public boolean can_be_lt = false;
  public boolean can_be_gt = false;

  // If set, these mean that the invariant is obvious as soon as the
  // specified flag is set.  For instance, we might set obvious_can_be_lt
  // for "b[0] cmp max(b)".  It would be interesting and relevant if we
  // found those quantities equal, but if the relationship can be "<", then
  // it is "<=" (because we know they can be "=="), so it's obvious and
  // uninteresting.
  // In other words, if the thing that's obviously possible hsn't yet
  // happened, then the invariant is interesting.

  public boolean only_check_eq;
  public boolean obvious_can_be_lt;
  public boolean obvious_can_be_gt;
  public boolean obvious_can_be_le;
  public boolean obvious_can_be_ge;

  public Invariant wrapper;

  public StringComparisonCore(Invariant wrapper) {
    this(wrapper, false, false, false, false, false);
  }

  public StringComparisonCore(Invariant wrapper, boolean only_eq) {
    this(wrapper, only_eq, false, false, false, false);
  }

  public StringComparisonCore(Invariant wrapper, boolean only_eq, boolean obvious_lt, boolean obvious_gt, boolean obvious_le, boolean obvious_ge) {
    this.wrapper = wrapper;
    only_check_eq = only_eq;
    obvious_can_be_lt = obvious_lt;
    obvious_can_be_gt = obvious_gt;
    obvious_can_be_le = obvious_le;
    obvious_can_be_ge = obvious_ge;
    if (only_eq || obvious_lt || obvious_gt || obvious_le || obvious_ge) {
      Global.partially_implied_invariants++;
    }
  }

  public Object clone() {
    try {
      StringComparisonCore result = (StringComparisonCore) super.clone();
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

  // for resurrection
  public void swap() {
    boolean tmp;

    tmp = can_be_lt;
    can_be_gt = can_be_lt;
    can_be_gt = tmp;

    tmp = obvious_can_be_lt;
    obvious_can_be_lt = obvious_can_be_gt;
    obvious_can_be_gt = tmp;

    tmp = obvious_can_be_le;
    obvious_can_be_le = obvious_can_be_ge;
    obvious_can_be_ge = tmp;
  }

  public InvariantStatus check_modified(String v1, String v2, int count) {
    return checkOrAddModified(v1, v2, count, false);
  }

  public InvariantStatus add_modified(String v1, String v2, int count) {
    return checkOrAddModified(v1, v2, count, true);
  }

  /**
   * @param add true if v1 and v2 are to be added to this false otherwise.
   */
  private InvariantStatus checkOrAddModified(String v1, String v2, int count, boolean add) {
    if ((v1 == null) || (v2 == null)) {
      return InvariantStatus.FALSIFIED;
    }

    boolean new_can_be_eq = can_be_eq;
    boolean new_can_be_lt = can_be_lt;
    boolean new_can_be_gt = can_be_gt;
    boolean changed = false;

    if (v1 == v2) {
      if (! can_be_eq) {
        new_can_be_eq = true;
        changed = true;
      }
    } else if (v1.compareTo(v2) < 0) {
      if (! can_be_lt) {
        new_can_be_lt = true;
        changed = true;
      }
    } else {
      if (! can_be_gt) {
        new_can_be_gt = true;
        changed = true;
      }
    }

    if (! changed) {
      return InvariantStatus.NO_CHANGE;
    }

    if ((new_can_be_lt && new_can_be_gt)
        || (only_check_eq && (new_can_be_lt || new_can_be_gt))
        || (obvious_can_be_lt && new_can_be_lt)
        || (obvious_can_be_gt && new_can_be_gt)
        || (obvious_can_be_le && new_can_be_lt && new_can_be_eq)
        || (obvious_can_be_ge && new_can_be_gt && new_can_be_eq)) {
      return InvariantStatus.FALSIFIED;
    }

    if (add) {
      can_be_eq = new_can_be_eq;
      can_be_lt = new_can_be_lt;
      can_be_gt = new_can_be_gt;
    }
    return InvariantStatus.WEAKENED;
  }

  // This is very tricky, because whether two variables are equal should
  // presumably be transitive, but it's not guaranteed to be so when using
  // this method and not dropping out all variables whose values are ever
  // missing.
  public double computeConfidence() {
    if (can_be_lt || can_be_gt) {
      return 1 - Math.pow(.5, wrapper.ppt.num_values());
    } else {
      if (can_be_eq) {
        // It's an equality invariant.  I ought to use the actual ranges somehow.
        // Actually, I can't even use this .5 test because it can make
        // equality non-transitive.
        // return Math.pow(.5, wrapper.ppt.num_values());
        return Invariant.CONFIDENCE_JUSTIFIED;
      } else {
        // None of the can_be_X's are true.
        // (We haven't seen any values yet.)
        return Invariant.CONFIDENCE_UNJUSTIFIED;
      }
    }
  }

  public String repr() {
    return "StringComparisonCore: "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + ",only_check_eq=" + only_check_eq
      + ",obvious_can_be_lt=" + obvious_can_be_lt
      + ",obvious_can_be_gt=" + obvious_can_be_gt
      + ",obvious_can_be_le=" + obvious_can_be_le
      + ",obvious_can_be_ge=" + obvious_can_be_ge;
  }

  // I could alternately take two variable names as arguments and return
  // the full formatted thing...
  /** Return a comparator such as "<=" or ">" or "==". **/
  public String format_comparator(OutputFormat format) {
    if (can_be_eq && (! can_be_gt) && (! can_be_lt)) {
      if (format == OutputFormat.IOA) {
        return "=";
      } else {
        return "==";
      }
    } else if (can_be_eq || can_be_gt || can_be_lt) {
      // TODO: reenable after making distribution.
      // Assert.assertTrue(can_be_lt || can_be_gt);
      String inequality = (can_be_lt ? "<" : can_be_gt ? ">" : "");
      String comparison = (can_be_eq ? "=" : "");
      return inequality + comparison;
    } else {
      return "?cmp?";
    }
  }

  public boolean isExact() {
    return (can_be_eq && (!can_be_lt) && (!can_be_gt));
  }

  public boolean isSameFormula(StringComparisonCore other)
  {
    return
      (can_be_eq == other.can_be_eq) &&
      (can_be_lt == other.can_be_lt) &&
      (can_be_gt == other.can_be_gt);
  }

  public boolean isExclusiveFormula(StringComparisonCore other)
  {
    return (! ((can_be_eq && other.can_be_eq)
               || (can_be_lt && other.can_be_lt)
               || (can_be_gt && other.can_be_gt)));
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


    // Yes, this comparison is obvious.  However:
    //  * we don't know whether it is an equality or a non-strict inequality.
    //  * we may wish to have the comparison on hand later, when we iterate
    //    over all StringComparison objects.
    // I should suppress this on *output*, not on computation.
    // Better yet:  suppress it as soon as it becomes obvious.
    // if ((seqvar1 != null) && (seqvar1 == var2.isDerivedSequenceMember())) {
    //   if ((var1.derived instanceof SequenceMax)
    //       || (var1.derived instanceof SequenceMin)
    //       || (var2.derived instanceof SequenceMax)
    //       || (var2.derived instanceof SequenceMin)) {
    //     return null;
    //   }
    // }
