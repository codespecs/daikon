package daikon.inv.unary.scalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.UnaryInvariant;
import plume.*;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import typequals.*;
*/

/**
 * Abstract base class for invariants over one numeric (scalar) variable, such as {@code x != 0}.
 */
public abstract class SingleScalar extends UnaryInvariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected SingleScalar(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ SingleScalar() {
    super();
  }

  public VarInfo var(
      /*>>>@GuardSatisfied @UnknownInitialization(SingleScalar.class) @Raw(SingleScalar.class) SingleScalar this*/) {
    return ppt.var_infos[0];
  }

  /**
   * Returns whether or not the specified types are valid for unary scalar. (Static version of
   * method.)
   */
  public static final boolean valid_types_static(VarInfo[] vis) {
    return ((vis.length == 1) && vis[0].file_rep_type.isScalar());
  }

  /** Returns whether or not the specified types are valid for unary scalar. */
  @Override
  public final boolean valid_types(VarInfo[] vis) {
    return valid_types_static(vis);
  }

  // Should never be called with modified == ValueTuple.MISSING_NONSENSICAL.
  // Subclasses need not override this except in special cases;
  // just implement @link{add_modified(Object,int)}.
  @Override
  public InvariantStatus add(/*@Interned*/ Object val, int mod_index, int count) {
    assert !falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    long value = ((Long) val).longValue();
    if (mod_index == 0) {
      return add_unmodified(value, count);
    } else {
      return add_modified(value, count);
    }
  }

  @Override
  public InvariantStatus check(/*@Interned*/ Object val, int mod_index, int count) {
    assert !falsified;
    assert (mod_index >= 0) && (mod_index < 2);
    long value = ((Long) val).longValue();
    if (mod_index == 0) {
      return check_unmodified(value, count);
    } else {
      return check_modified(value, count);
    }
  }

  /**
   * Similar to {@link #check_modified} except that it can change the state of the invariant if
   * necessary. If the invariant doesn't have any state, then the implementation should simply call
   * {@link #check_modified}. This method need not check for falsification; that is done by the
   * caller.
   */
  public abstract InvariantStatus add_modified(long value, int count);

  /** By default, do nothing if the value hasn't been seen yet. Subclasses can override this. */
  public InvariantStatus add_unmodified(long value, int count) {
    // System.out.println("SingleScalar.add_unmodified " + ppt.name() + ": parent=" + ppt.parent);
    return InvariantStatus.NO_CHANGE;
  }

  /**
   * Presents a sample to the invariant. Returns whether the sample is consistent with the
   * invariant. Does not change the state of the invariant.
   *
   * @param count how many identical samples were observed in a row. For example, three calls to
   *     check_modified with a count parameter of 1 is equivalent to one call to check_modified with
   *     a count parameter of 3.
   * @return whether or not the sample is consistent with the invariant
   */
  public abstract InvariantStatus check_modified(long value, int count);

  public InvariantStatus check_unmodified(long value, int count) {
    return InvariantStatus.NO_CHANGE;
  }

  // This has no additional suppression factories beyond those of Invariant.

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
