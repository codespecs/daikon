package daikon.inv.scalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

// I think this is likely to disappear, except possibly as a place to keep
// common data like minimum and maximum.

public abstract class SingleScalar extends Invariant {

  protected SingleScalar(PptSlice ppt_) {
    super(ppt_);
    // System.out.println("Created SingleScalar invariant " + this + " at " + ppt_);
  }

  public VarInfo var() {
    return ppt.var_infos[0];
  }

  // Should never be called with modified == ValueTuple.MISSING.
  // Subclasses need not override this except in special cases;
  // just implement @link{add_modified(Object,int)}.
  public void add(int value, int modified, int count) {
    Assert.assert((modified == ValueTuple.MODIFIED)
		  || (modified == ValueTuple.UNMODIFIED));
    if (modified == ValueTuple.MODIFIED) {
      if (! no_invariant) {
        add_modified(value, count);
      }
    } else {
      add_unmodified(value, count);
    }
  }

  /**
   * This method need not check for no_invariant;
   * that is done by the caller.
   */
  public abstract void add_modified(int value, int count);

  /**
   * By default, do nothing if the value hasn't been seen yet.
   * Subclasses can overrided this.
   */
  public void add_unmodified(int value, int count) {
    return;
  }


/// Old version that used VarInfos rather than using a parent which is a
/// PptSlice.
//   VarInfo var_info;
//   public SingleScalar(Ppt ppt_, VarInfo var_info_) {
//     super(ppt_);
//     var_info = var_info_;
//   }
//   public boolean usesVar(VarInfo vi) {
//     return (var_info == vi);
//   }
//   public abstract void add(ValueTuple vt, int count);

}


//     def format(self, arg_tuple=None):
//         if arg_tuple == None:
//             if self.var_infos:
//                 arg = self.var_infos[0].name
//             # not sure whether this is the right thing, but oh well
//             else:
//                 arg = "var"
//         else:
//             (arg,) = arg_tuple
//
//         as_base = invariant.format(self, arg)
//         if as_base:
//             return as_base
//         self.unconstrained_internal = false
//
//         suffix = " \t(%d values" % (self.values,)
//         if self.can_be_None:
//             suffix += ", can be None)"
//         else:
//             suffix += ")"
//
//         if self.modulus and self.modulus_justified():
//             return arg + " = %d (mod %d)" % self.modulus + suffix
//         elif self.nonmodulus and self.nonmodulus_justified():
//             return arg + " != %d (mod %d)" % self.nonmodulus + suffix
//
//         nonzero = ((self.min < 0) and (self.max > 0)
//                    and (not self.can_be_zero) and self.nonzero_justified())
//
//         if self.min_justified and self.max_justified:
//             result = " in [%s..%s]" % (self.min, self.max)
//             if nonzero:
//                 result = " nonzero" + result
//             return arg + result + suffix
//         if self.min_justified and (self.min != 0 or not self.nonnegative_obvious):
//             result = "%s >= %s" % (arg, self.min)
//             if nonzero:
//                 result += " and nonzero"
//             return result + suffix
//         if self.max_justified:
//             result = "%s <= %s" % (arg, self.max)
//             if nonzero:
//                 result += " and nonzero"
//             return result + suffix
//         if nonzero:
//             return arg + "!= 0" + suffix
//
//         if self.one_of and not self.can_be_None:
//             return "%s in %s" % (arg, util.format_as_set(self.one_of))
//
//         self.unconstrained_internal = true
//         return arg + " unconstrained" + suffix
//
//     def diff(self, other):
//         # print "diff(single_scalar_numeric_invariant)"
//         inv1 = self
//         inv2 = other
//
//         # If they print the same, then make them compare the same
//         if diffs_same_format(inv1, inv2):
//             return None
//
//         as_base = invariant.diff(inv1, inv2)
//         if as_base:
//             return as_base
//
//         min_missing = ((inv1.min_justified and not inv2.min_justified)
//                        or (inv2.min_justified and not inv1.min_justified))
//         min_different = (inv1.min_justified and inv2.min_justified
//                          and inv1.min != inv2.min)
//         max_missing = ((inv1.max_justified and not inv2.max_justified)
//                        or (inv2.max_justified and not inv1.max_justified))
//         max_different = (inv1.max_justified and inv2.max_justified
//                          and (inv1.max != inv2.max))
//         # print "max_different=%s" % (max_different,), inv1.max_justified, inv2.max_justified, inv1.max, inv2.max
//         nzj1 = inv1.nonzero_justified()
//         nzj2 = inv1.nonzero_justified()
//         zero_different = (nzj1 and not nzj2) or (nzj2 and not nzj1)
//
//         modulus_different = (inv1.modulus != inv2.modulus)
//         nonmodulus_different = (inv1.nonmodulus != inv2.nonmodulus)
//
//         if result == []:
//             return None
//         return string.join(result, ", ")
