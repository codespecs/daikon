package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.sequence.*;
import daikon.derive.binary.*;


import utilMDE.*;

import java.util.*;
import java.util.logging.*;
import java.io.Serializable;


/**
 * Generates SuppressionLink objects.  Responsible for checking if an
 * invariant is suppressed.  Meant to be inherited from so each
 * subclass suppresses some classes of invariants.  Factory's should
 * be immutable.
 *
 * How child SuppressionFactories are added to the system: see
 * this package's description.
 **/

public abstract class SuppressionFactory implements Serializable {


  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031024L;

  /**
   * General debug tracer.
   **/
  public static final Logger debug =
    Logger.getLogger ("daikon.suppress.SuppresionFactory");


  // The default gives the hashcode, which is bad for comparisons.
  public String toString() {
    return this.getClass().getName();
  }


  /**
   * Check whether this type of suppression applies to a given
   * invariant at a given program point.  If so, return a
   * SuppressionLink.  Otherwise return null.  A SuppressionFactory
   * should not attempt to search through the invariants by itself,
   * but should use SuppressionTemplate.fill.
   *
   * Does not link the invariants.
   * @param inv the Invariant that may be suppressed.
   * @return null if suppression method doesn't apply.
   **/
  public abstract SuppressionLink generateSuppressionLink (Invariant inv);


  /**
   * Generate a SuppressionLink from an unfilled template, by asking an
   * inv's PptTopLevel to fill in the template.  If the template filling is
   * successful, return a SuppressionLink.
   * This is primarily a helper method for use by generateSuppressionLink.
   *
   * @param supt The suppression template to fill
   * @param inv the Invariant that is potentially being suppressed.
   * @return the matching suppression link or null.
   **/
  public static SuppressionLink linkFromUnfilledTemplate (SuppressionTemplate supt,
                                                   Invariant inv) {
    Assert.assertTrue(supt.filled == false);
    if (inv.logOn())
      inv.log ("Suppression Template - " + supt.searchString());
    SuppressionLink result = null;
    if (supt.fill(inv.ppt.parent)) {
      result = linkFromFilledTemplate (supt, inv);
    }
    if (inv.logOn()) {
      if (result != null) {
        inv.log ("Found Template Match " + supt.results[0].format());
      } else {
        inv.log ("No Template Match found");
      }
    }
    return result;
  }


  /**
   * Generate a SuppressionLink from a filled template.  Never returns null.
   * This is used by Factory's that fill their templates in a way that the
   * linkFromUnfilledTemplate method is inappropriate for them (e.g. a
   * SuppressionLink might not be generated for all filled templates,
   * so a Factory scans a list of filled templates).
   * This is primarily a helper method for use by generateSuppressionLink.
   *
   * @param supTemplate a filled template.
   * @param inv the Invariant that is being suppressed
   **/
  protected static SuppressionLink linkFromFilledTemplate (SuppressionTemplate supTemplate,
                                                    Invariant inv) {
    Assert.assertTrue (supTemplate.filled, "Template must be filled");
    SuppressionLink sl = new SuppressionLink (inv, supTemplate.results);
    return sl;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Suppression utilities
  ///

  // All of these need to be generalized to floating-point numbers as well.

  private static transient SuppressionTemplate supTemplate_findLessEqualInt = new SuppressionTemplate(1);
  private static transient SuppressionTemplate supTemplate_findEqualInt = new SuppressionTemplate(1);
  private static transient SuppressionTemplate supTemplate_findConstantInt = new SuppressionTemplate(1);
  private static transient SuppressionTemplate supTemplate_findInRange = new SuppressionTemplate(2);
  private static transient SuppressionTemplate supTemplate_findUpperBound = new SuppressionTemplate(1);
  private static transient SuppressionTemplate supTemplate_findLowerBound = new SuppressionTemplate(1);
  private static transient SuppressionTemplate supTemplate_findEmptyArray = new SuppressionTemplate(1);


  /**
   * Return a SuppressionLink suppressing the invariant by
   * "v1 < v2 + interval"; equivalently, "v1 - v2 <= interval".
   * When interval is 0, this is equivalent to "v1 < v2" (or "v1 <= v2").
   * Properly looks for "v2 > v1" as well.
   * Only works on integers because this is used for array indexing.
   * @param inv The invariant whose ppt in which we are looking for
   * the < relationship.
   * @param interval The interval that the two VarInfos have to vary by (at least).
   **/
  public static SuppressionLink findLessEqualInt (Invariant inv, VarInfo v1, VarInfo v2, int interval) {

    SuppressionTemplate supTemplate = supTemplate_findLessEqualInt;

    // Why is looking for LessThan necessary?  Shouldn't LessEqual subsume that?
    // In particular, what is wrong with
    //     if (interval <= 0) {
    //       supTemplate.set(0, IntLessEqual.class, var1, var2);
    //     } else if (interval == -1) {
    //       supTemplate.set(0, IntLessThan.class, var1, var2);
    //     } else {
    //       return null;
    //     }
    //     return linkFromUnfilledTemplate (supTemplate, inv);
    // ?

    {
      supTemplate.set(0, IntLessThan.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntLessThan resultInv = (IntLessThan) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        // "var1 <= var2 + -1" is equivalent to "var1 < var2" (for integers)
        if (leftResult == resultInv.var1()
            && interval <= -1) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    {
      supTemplate.set(0, IntLessEqual.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntLessEqual resultInv = (IntLessEqual) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        if (leftResult == resultInv.var1()
            && interval <= 0) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    {
      supTemplate.set(0, IntGreaterThan.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntGreaterThan resultInv = (IntGreaterThan) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        // "var1 <= var2 + -1" is equivalent to "var1 < var2" (for integers)
        if (leftResult == resultInv.var2()
            && interval <= -1) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    {
      supTemplate.set(0, IntGreaterEqual.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntGreaterEqual resultInv = (IntGreaterEqual) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        if (leftResult == resultInv.var2()
            && interval <= 0) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    return null;
  }


  // This one's so small it hardly seems worth writing...
  // Return a SuppressionLink suppressing the invariant by "var1 = var2".
  public static SuppressionLink findEqualInt (Invariant inv, VarInfo var1, VarInfo var2) {
    if (inv.logOn())
      inv.log ("seeking " + var1.name.name() + " = " + var2.name.name());

    SuppressionTemplate supTemplate = supTemplate_findEqualInt;
    supTemplate.set(0, IntEqual.class, var1, var2);
    return linkFromUnfilledTemplate (supTemplate, inv);
  }


  // Return a SuppressionLink suppressing the invariant by "var == constant".
  public static SuppressionLink findConstantInt (Invariant inv, VarInfo var, long constant) {
    SuppressionTemplate supTemplate = supTemplate_findConstantInt;
    supTemplate.set(0, OneOfScalar.class, var);
    SuppressionLink sl = linkFromUnfilledTemplate (supTemplate, inv);
    if (sl == null) {
      return null;
    }

    // Check the constant
    OneOfScalar oos = (OneOfScalar) supTemplate.results[0];
    if (oos.num_elts() != 1) {
      return null;
    }
    if (((Long) oos.elt()).longValue() == constant) {
      return sl;
    }
    return null;
  }

  public static SuppressionLink findInRange (Invariant inv, VarInfo var, int lowerbound, int upperbound) {
    SuppressionTemplate supTemplate = supTemplate_findInRange;
    supTemplate.set(0, LowerBound.class, var);
    supTemplate.set(1, UpperBound.class, var);

    // Find a match
    SuppressionLink sl = linkFromUnfilledTemplate (supTemplate, inv);
    if (sl == null) {
      return null;
    }

    // Check the bounds
    LowerBound lb = (LowerBound) supTemplate.results[0];
    long lbound = lb.min();
    UpperBound ub = (UpperBound) supTemplate.results[1];
    long ubound = ub.max();
    if ((ubound <= upperbound) && (lbound <= lowerbound)) {
      return (sl);
    }

    return null;
  }


  // Return a SuppressionLink suppressing the invariant by "var <= limit".
  public static SuppressionLink findUpperBound (Invariant inv, VarInfo var, int limit) {

    if (inv.logOn())
      inv.log ("seeking " + var.name.name() + " <= " + limit);

    SuppressionTemplate supTemplate = supTemplate_findUpperBound;
    supTemplate.set(0, UpperBound.class, var);

    // Find a match
    SuppressionLink sl = linkFromUnfilledTemplate (supTemplate, inv);
    if (sl == null) {
      return null;
    }

    // Check the bound
    UpperBound ub = (UpperBound) supTemplate.results[0];
    long bound = ub.max();
    if (bound <= limit) {
      return (sl);
    }
    return null;
  }


  // Return a SuppressionLink suppressing the invariant by "var >= limit".
  public static SuppressionLink findLowerBound (Invariant inv, VarInfo var, int limit) {

    if (inv.logOn())
      inv.log ("seeking " + var.name.name() + " <= " + limit);

    SuppressionTemplate supTemplate = supTemplate_findLowerBound;
    supTemplate.set(0, LowerBound.class, var);

    // Find a match
    SuppressionLink sl = linkFromUnfilledTemplate (supTemplate, inv);
    if (sl == null) {
      return null;
    }

    // Check the bound
    LowerBound ub = (LowerBound) supTemplate.results[0];
    long bound = ub.min();
    if (bound <= limit) {
      return (sl);
    }
    return null;
  }


  public static SuppressionLink findEmptyArray (Invariant inv, VarInfo array_vi) {

    if (inv.logOn())
      inv.log ("Considering suppression of " + inv.format() + " by findEmptyArray_arraylength");

    SuppressionLink sl;

    sl = findEmptyArray_via_OneOfSequence(inv, array_vi);
    if (sl != null) {
      return sl;
    }

    sl = findEmptyArray_arraylength(inv, array_vi);
    if (sl != null) {
      return sl;
    }

    return null;
  }


  // TODO:  Factor this out by types and turn it into a #included file.

  // This does not successfully determine that A[0..zero-1] is empty, where
  // zero=0, because A[0..zero-1] is a no good derived variable (?).
  public static SuppressionLink findEmptyArray_via_OneOfSequence (Invariant inv, VarInfo var) {

    if (inv.logOn())
      inv.log ("findEmptyArray");
    if (inv.logOn())
      inv.log ("seeking " + var.name.name() + " == []");

    SuppressionTemplate supTemplate = supTemplate_findEmptyArray;

    supTemplate.set(0, OneOfSequence.class, var);
    SuppressionLink sl = linkFromUnfilledTemplate (supTemplate, inv);
    if (sl != null) {
      OneOfSequence oos = (OneOfSequence) supTemplate.results[0];
      if (oos.num_elts() == 1) {
        long[] oneValue = (long[]) oos.elt();
        if (oneValue.length == 0)
          return sl;
      }
    }

      return null;
  }


  // Determine that A[0..zero-1] is empty, where zero=0.
  public static SuppressionLink findEmptyArray_arraylength (Invariant inv, VarInfo array_vi) {

    if (inv.logOn())
      inv.log ("Considering suppression of " + inv.format() + " by findEmptyArray_arraylength");

    // This implementation doesn't need to be special-cased for each type.

    // array_vi is either a subsequence (such as a[i..j]) or a complete array (a[]).
    if (array_vi.derived == null) {
      // array_vi is a complete array
      VarInfo array_size_vi = array_vi.sequenceSize();
      return findUpperBound(inv, array_size_vi, 0);
    }
    Assert.assertTrue(array_vi.derived != null);
    if (array_vi.derived instanceof SequenceSubsequence) {
      // array_vi is a subsequence of the form a[i..] or a[..j]
      if (inv.logOn())
        inv.log ("Derived var " + array_vi.name.name());
      SequenceSubsequence ss = (SequenceSubsequence) array_vi.derived;
      if (ss.from_start) {
        // variable of the form a[0..n] (if ss.index_shift == 0)
        // or a[0..n-1] (if ss.index_shift == -1)
        return findUpperBound(inv, ss.sclvar(), -1-ss.index_shift);
      } else {
        // variable of the form a[m..] (if ss.index_shift == 0)
        // or a[m+1..] (if ss.index_shift == -1); use a different factory
      return null;
      }
    }
    // array_vi is not of the form handled by this routine.
      return null;

  }



/// This is a *far* too complicated way to check whether an array is empty.
//   /**
//    * Suppress invariants of the form 'A elements != 0' where A is
//    * always of size 0 and A has form "a[]" or "a[..n]".
//    * (For example, suppress if A = "a[]" and 'size[a] == 0' invariant exists.)
//    */
//   public static SuppressionLink short_array_arraylength_suppress (CLASSNAME inv,
//                   SuppressionTemplate supTemplate, SuppressionFactory sfact) {
//
//     if (logOn())
//       inv.log ("Considering suppression of " + inv.format() + " by short_array_arraylength");
//
//     VarInfo array_vi = inv.var();
//     // array_vi is either a subsequence (such as a[i..j]) or a complete array (a[]).
//     if (array_vi.derived == null) {
//       // array_vi is a complete array
//       VarInfo array_size_vi = array_vi.sequenceSize();
//       return sfact.findUpperBound(inv, array_size_vi, 0);
//     }
//     Assert.assertTrue(array_vi.derived != null);
//     if (array_vi.derived instanceof SequenceSubsequence) {
//       // array_vi is a subsequence of the form a[i..] or a[..j]
//       if (logOn())
//         inv.log ("Derived var " + array_vi.name.name());
//       SequenceSubsequence ss = (SequenceSubsequence) array_vi.derived;
//       if (ss.from_start) {
//         // variable of the form a[0..n] (if ss.index_shift == 0)
//         // or a[0..n-1] (if ss.index_shift == -1)
//         return sfact.findUpperBound(inv, ss.sclvar(), -ss.index_shift);
//       } else {
//         // variable of the form a[m..] (if ss.index_shift == 0)
//         // or a[m+1..] (if ss.index_shift == -1); use a different factory
//         return null;
//       }
//     }
//     // array_vi is not of the form handled by this routine.
//     return null;
//   }
//
//   /**
//    * Suppress invariants of the form 'A elements != 0' where A is
//    * always of size 0 and A has form "a[m..]" or "a[m..n]".
//    * (For example, suppress if A = "a[]" and 'size[a] == 0' invariant exists.)
//    */
//   public static SuppressionLink empty_array_arraylimits_suppress (CLASSNAME inv,
//                   SuppressionTemplate supTemplate, SuppressionFactory sfact) {
//
//     if (logOn())
//       inv.log ("Considering suppression of " + inv.format() + " by short_array_arraylimits");
//
//     VarInfo array_vi = inv.var();
//     // array_vi is either a subsequence (such as a[i..j]) or a complete array (a[]).
//     if (array_vi.derived == null) {
//       return null;
//     }
//     Assert.assertTrue(array_vi.derived != null);
//     if (logOn())
//       inv.log ("Derived var " + array_vi.name.name());
//     // The derived variable is a[(li+lio)..(ui+uio)].
//     VarInfo startindex;
//     int start_offset;
//     VarInfo endindex;
//     int end_offset;
//     if (array_vi.derived instanceof SequenceSubsequence) {
//       // A subsequence of the form a[i..] or a[..j]
//       SequenceSubsequence ss = (SequenceSubsequence) array_vi.derived;
//       if (ss.from_start) {
//         // variable of the form a[0..n] (if ss.index_shift == 0)
//         // or a[0..n-1] (if ss.index_shift == -1); use a different factory
//         return null;
//       }
//       // variable of the form a[m..] (if ss.index_shift == 0)
//       // or a[m+1..] (if ss.index_shift == -1)
//       startindex = ss.sclvar();
//       start_offset = ss.index_shift;
//       endindex = ss.seqvar().sequenceSize();
//       end_offset = 0;
//     } else if (array_vi.derived instanceof SEQUENCESCALARARBITRARYSUBSEQUENCE) {
//       // A subsequence of the form a[i..j]
//       SEQUENCESCALARARBITRARYSUBSEQUENCE ssas = (SEQUENCESCALARARBITRARYSUBSEQUENCE) array_vi.derived;
//       startindex = ssas.startvar();
//       start_offset = ssas.left_closed ? 0 : -1;
//       endindex = ssas.endvar();
//       end_offset = ssas.right_closed ? 0 : +1;
//     } else {
//       if (logOn())
//         inv.log("wrong kind of derived variable: " + array_vi.name.name());
//       return null;
//     }
//
//     if (end_offset == start_offset) {
//       // return true if startindex = endindex
//       return sfact.findEqualInt(inv, startindex, endindex);
//     } else {
//       if (logOn())
//         inv.log("differing start and end offsets:"
//                 + " " + startindex.name.name() + " + " + start_offset
//                 + " " + endindex.name.name() + " + " + end_offset);
//       return null;
//     }
//   }


}
