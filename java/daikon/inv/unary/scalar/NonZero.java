package daikon.inv.unary.scalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.binary.sequenceScalar.*;
import daikon.derive.unary.*;

import java.util.*;

import utilMDE.*;

// This also serves as NonNull.

public class NonZero extends SingleScalar {

  private static boolean debugNonZero = false;

  long min = Long.MAX_VALUE;
  long max = Long.MIN_VALUE;

  // If nonzero, use this as the range instead of the actual range.
  // This lets one use a specified probability of nonzero (say, 1/10
  // for pointers).
  long override_range = 0;
  boolean pointer_type = false;

  private NonZero(PptSlice ppt) {
    super(ppt);
  }

  public static NonZero instantiate(PptSlice ppt) {
    if (debugNonZero || ppt.debugged) {
      System.out.println("NonZero.instantiate(" + ppt.name + ")");
    }

    NonZero result = new NonZero(ppt);
    if (! ppt.var_infos[0].type.isIntegral()) {
      result.pointer_type = true;
      result.override_range = 3;
    }
    return result;
  }

  public String repr() {
    return "NonZero" + varNames() + ": "
      + !no_invariant + ",min=" + min + ",max=" + max;
  }

  private String zero() { return pointer_type ? "null" : "0"; }

  public String format() {
    return var().name.name() + " != " + zero();
  }

  public String format_esc() {
    return var().name.esc_name() + " != " + zero();
  }

  public String format_simplify() {
    return "(NEQ " + var().name.simplify_name() + " " + zero() + ")";
  }

  public void add_modified(long v, int count) {
    // The min and max tests will simultaneoulsy succeed exactly once (for
    // the first value).
    if (v == 0) {
      destroy();
      return;
    }
    if (v < min) min = v;
    if (v > max) max = v;
  }

  protected double computeProbability() {
    Assert.assert(! no_invariant);
    // Maybe just use 0 as the min or max instead, and see what happens:
    // see whether the "nonzero" invariant holds anyway.  (Perhaps only
    // makes sense to do if the {Lower,Upper}Bound invariant doesn't imply
    // the non-zeroness.)  In that case, do still check for no values yet
    // received.
    if ((override_range == 0) && ((min > 0) || (max < 0)))
      return Invariant.PROBABILITY_UNKNOWN;
    else {
      long range;
      if (override_range != 0) {
        range = override_range;
      } else {
        long modulus = 1;
        {
          for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
            Invariant inv = (Invariant) itor.next();
            if ((inv instanceof Modulus) && inv.enoughSamples()) {
              modulus = ((Modulus) inv).modulus;
              break;
            }
          }
        }
        // Perhaps I ought to check that it's possible (given the modulus
        // constraints) for the value to be zero; otherwise, the modulus
        // constraint implies non-zero.
        range = (max - min + 1) / modulus;
      }
      double probability_one_elt_nonzero = 1 - 1.0/range;
      // This could underflow; so consider doing
      //   double log_probability = self.samples*math.log(probability);
      // then calling Math.exp (if the value is in the range that wouldn't
      // cause underflow).
      return Math.pow(probability_one_elt_nonzero, ppt.num_mod_non_missing_samples());
    }
  }

  public boolean isObviousImplied() {
    VarInfo var = var();

    // System.out.println("isObviousImplied: " + format());

    // For every EltNonZero at this program point, see if this variable is
    // an obvious member of that sequence.
    PptTopLevel parent = (PptTopLevel)ppt.parent;
    for (Iterator itor = parent.invariants_iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if ((inv instanceof EltNonZero) && inv.enoughSamples()) {
        VarInfo v1 = var();
        VarInfo v2 = inv.ppt.var_infos[0];
        // System.out.println("NonZero.isObviousImplied: calling Member.isObviousMember(" + v1.name + ", " + v2.name + ")");
        // Don't use isEqualToObviousMember:  that is too subtle
        // and eliminates desirable invariants such as "return != null".
        if (Member.isObviousMember(v1, v2)) {
          // System.out.println("NonZero.isObviousImplied: Member.isObviousMember(" + v1.name + ", " + v2.name + ") = true");
          return true;
        }
      }
    }

    if ((var.derived != null)
        && (var.derived instanceof SequenceInitial)) {
      SequenceInitial si = (SequenceInitial) var.derived;
      if (si.index == 0) {

        // For each sequence variable, if var is an obvious member, and
        // the sequence has the same invariant, then this one is obvious.
        PptTopLevel pptt = (PptTopLevel) ppt.parent;
        for (int i=0; i<pptt.var_infos.length; i++) {
          VarInfo vi = pptt.var_infos[i];
          if (Member.isObviousMember(var, vi)) {
            PptSlice1 other_slice = pptt.findSlice(vi);
            if (other_slice != null) {
              SeqIndexNonEqual sine = SeqIndexNonEqual.find(other_slice);
              if ((sine != null) && sine.enoughSamples()) {
                return true;
              }
            }
          }
        }
      }
    }

    return false;
  }


  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof NonZero);
    return true;
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof OneOfScalar) {
      OneOfScalar oos = (OneOfScalar) other;
      if ((oos.num_elts() == 1) && (((Long)oos.elt()).longValue() == 0)) {
        return true;
      }
    }
    return false;
  }


}
