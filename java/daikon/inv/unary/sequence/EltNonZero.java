package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;

import utilMDE.*;

import java.util.*;

// States that the value is one of the specified values.

// This subsumes an "exact" invariant that says the value is always exactly
// a specific value.  Do I want to make that a separate invariant
// nonetheless?  Probably not, as this will simplify implication and such.

public final class EltNonZero
  extends SingleSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff EltNonZero invariants should be considered.
   **/
  public static boolean dkconfig_enabled = true;

  long min = Long.MAX_VALUE;
  long max = Long.MIN_VALUE;

  // If nonzero, use this as the range instead of the actual range.
  // This lets one use a specified probability of nonzero (say, 1/10
  // for pointers).
  int override_range = 0;
  boolean pointer_type = false;

  EltNonZero(PptSlice ppt) {
    super(ppt);
  }

  public static EltNonZero instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    EltNonZero result = new EltNonZero(ppt);
    if (! ppt.var_infos[0].type.baseIsIntegral()) {
      result.pointer_type = true;
      result.override_range = 3;
    }
    // System.out.println("EltNonZero.instantiate: " + result.format());
    return result;
  }

  public String repr() {
    return "EltNonZero" + varNames() + ": "
      + !no_invariant + ",min=" + min + ",max=" + max;
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.ESCJAVA) return format_esc();
    if (format == OutputFormat.IOA) return format_ioa();
    if (format == OutputFormat.SIMPLIFY) return format_simplify();

    return format_unimplemented(format);
  }

  public String format_daikon() {
    return var().name + " elements != " + (pointer_type ? "null" : "0");
  }

  public String format_esc() {
    
    //ESC can't recognize the .containsNull (Nii)
    //  if (pointer_type) {
    //        if (var().name instanceof VarInfoName.Elements) {
    //          VarInfoName term = ((VarInfoName.Elements) var().name).term;
    //        	return term.esc_name() + ".containsNull == false"; 
    //        }
    //        if (! (var().name instanceof VarInfoName.Slice)) {
    //  	 Calling var().name.esc_name() will always throw an
    //  	 exception, since var() is certainly a sequence.
    //  	 return var().name.esc_name() + ".containsNull == false";
    //        }
    //      }
    String[] form =
      VarInfoName.QuantHelper.format_esc(new VarInfoName[]
	{ var().name });
    return form[0] + "(" + form[1] + " != " + (pointer_type ? "null" : "0") + ")" + form[2];
  }

  /* IOA */
  public String format_ioa() {
    VarInfoName.QuantHelper.IOAQuantification quant = new VarInfoName.QuantHelper.IOAQuantification (var ());
    String result = quant.getQuantifierExp() + quant.getVarName(0) + " \\in " +
      var().name.ioa_name() + " => " + quant.getVarIndexed(0) + "~=";
    if (pointer_type) {
      return result + "null" + quant.getClosingExp();
    } else {
      return result + "0" + quant.getClosingExp();
    }
  }

  public String format_simplify() {
    String[] form =
      VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
	{ var().name });
    return form[0] + "(NEQ " + form[1] + " " + (pointer_type ? "null" : "0") + ")" + form[2];
  }

  public void add_modified(long[] a, int count) {
    for (int ai=0; ai<a.length; ai++) {
      long v = a[ai];

      // The min and max tests will simultaneoulsy succeed exactly once (for
      // the first value).
      if (v == 0) {
	flowThis();
        destroy();
        return;
      }
      // XXX; uh oh -- flowing these is bad stuff; maybe search for
      // upper / lower bound instead when computing probability
      if (v < min) min = v;
      if (v > max) max = v;
    }
  }

  protected double computeProbability() {
    Assert.assert(! no_invariant);
    // Maybe just use 0 as the min or max instead, and see what happens:
    // see whether the "nonzero" invariant holds anyway.  (Perhaps only
    // makes sense to do if the {Lower,Upper}Bound invariant doesn't imply
    // the non-zeroness.)  In that case, do still check for no values yet
    // received.
    if ((override_range == 0) && ((min > 0) || (max < 0)))
      return Invariant.PROBABILITY_UNJUSTIFIED;
    else {
      long range;
      if (override_range != 0) {
        range = override_range;
      } else {
        int modulus = 1;

        // I need to come back and make this work.
        // {
        //   for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
        //     Invariant inv = (Invariant) itor.next();
        //     if ((inv instanceof Modulus) && inv.enoughSamples()) {
        //       modulus = ((Modulus) inv).modulus;
        //       break;
        //     }
        //   }
        // }

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

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof EltNonZero);
    return true;
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof EltOneOf) {
      EltOneOf eoo = (EltOneOf) other;
      if ((eoo.num_elts() == 1) && (((Long)eoo.elt()).longValue() == 0)) {
        return true;
      }
    }
    return false;
  }

  public boolean isObviousImplied() {
    // For every other EltNonZero at this program point, see if there is a
    // subsequence relationship between that array and this one.

    PptTopLevel parent = (PptTopLevel)ppt.parent;
    for (Iterator itor = parent.invariants_iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if ((inv instanceof EltNonZero) && (inv != this) && inv.enoughSamples()) {
        VarInfo v1 = var();
        VarInfo v2 = inv.ppt.var_infos[0];
        if (SubSequence.isObviousDerived(v1, v2)) {
          // System.out.println("obvious: " + format() + "   because of " + inv.format());
          return true;
        }

        boolean this_var_first = (v1.varinfo_index < v2.varinfo_index);
        if (! this_var_first) { VarInfo temp = v1; v1 = v2; v2 = temp; }
        Assert.assert(v1.varinfo_index < v2.varinfo_index);
        PptSlice2 slice_2seq = parent.findSlice(v1, v2);
        if (slice_2seq == null) {
          // System.out.println("EltNonZero.isObviousImplied: no slice for " + v1.name + ", " + v2.name);
        } else  {
          // slice_2seq != null
          SubSequence ss = SubSequence.find(slice_2seq);
          if (ss == null) {
            // System.out.println("EltNonZero.isObviousImplied: no SubSequence for " + v1.name + ", " + v2.name);
          } else {
            // System.out.println("EltNonZero.isObviousImplied: found SubSequence: " + ss.repr());
            if (this_var_first
                ? ss.var1_in_var2
                : ss.var2_in_var1) {
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  // Look up a previously instantiated invariant.
  public static EltNonZero find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof EltNonZero)
        return (EltNonZero) inv;
    }
    return null;
  }

}
