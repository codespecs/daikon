package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.sequenceScalar.*;
import daikon.inv.unary.sequence.*;
import daikon.derive.binary.*;


import utilMDE.*;


// Also serves as NonAliased.
public final class NonEqual extends TwoScalar {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;
  public static boolean dkconfig_integral_only = false;

  public NonEqualCore core;

  protected NonEqual(PptSlice ppt) {
    super(ppt);
    int override_range = 0;
    if (! ppt.var_infos[0].type.isIntegral()) {
      override_range = 10;
    }
    core = new NonEqualCore(this, override_range);
  }

  public static NonEqual instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;

    // Perhaps do not instantiate unless the variables have the same
    // type; in particular, nonequal for Object variables is not so
    // likely to be of interest.
    boolean integral = ppt.var_infos[0].type.isIntegral() && ppt.var_infos[1].type.isIntegral();
    if (dkconfig_integral_only && !integral) {
      return null;
    }

    NonEqual result = new NonEqual(ppt);
    // System.out.println("NonEqual override_range = " + result.core.override_range + " for " + ppt.name);
    return result;
  }

  protected Object clone() {
    NonEqual result = (NonEqual) super.clone();
    result.core = (NonEqualCore) core.clone();
    result.core.wrapper = result;
    return result;
  }

  protected Invariant resurrect_done(int[] permutation) {
    core.permute(permutation);
    return this;
  }

  public String repr() {
    return "NonEqual" + varNames() + ": "
      + core.repr()
      + ",no_invariant=" + no_invariant;
  }

  public String format() {
    return var1().name.name() + " != " + var2().name.name();
  }

  public String format_esc() {
    return var1().name.esc_name() + " != " + var2().name.esc_name();
  }

  /* IOA */
  public String format_ioa(String classname) {
    return var1().name.ioa_name(classname)+" ~= "+var2().name.ioa_name(classname);
  }

  public String format_simplify() {
    return "(NEQ " + var1().name.simplify_name() + " " + var2().name.simplify_name() + ")";
  }

  public void add_modified(long v1, long v2, int count) {
    if (ppt.debugged) {
      System.out.println("NonEqual" + ppt.varNames() + ".add_modified("
                         + v1 + "," + v2 + ", count=" + count + ")");
    }
    core.add_modified(v1, v2, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return core.isExact();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((NonEqual) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof IntComparison) {
      if (((IntComparison)other).isExact()) {
        return true;
      }
    }
    return false;
  }

  public boolean isObviousImplied() {
    // If we are comparing a sequence element to it index, then
    // check for SeqIndexNonEqual.

    VarInfo v1 = var1();
    VarInfo v2 = var2();

    VarInfo seq = null;
    VarInfo seq_elt = null;
    if ((v1.derived != null)
        && (v1.derived instanceof SequenceScalarSubscript)) {
      SequenceScalarSubscript sss = (SequenceScalarSubscript) v1.derived;
      if (sss.sclvar() == v2) {
        seq = sss.seqvar();
        seq_elt = v1;
      }
    }
    if ((v2.derived != null)
        && (v2.derived instanceof SequenceScalarSubscript)) {
      SequenceScalarSubscript sss = (SequenceScalarSubscript) v2.derived;
      if (sss.sclvar() == v1) {
        seq = sss.seqvar();
        seq_elt = v2;
      }
    }
    if (seq_elt != null) {
      // For each sequence variable, if seq_elt is an obvious member, and
      // the sequence has the same invariant, then this one is obvious.
      PptTopLevel pptt = (PptTopLevel) ppt.parent;
      for (int i=0; i<pptt.var_infos.length; i++) {
        VarInfo vi = pptt.var_infos[i];
        if (Member.isObviousMember(seq_elt, vi)) {
          PptSlice1 other_slice = pptt.findSlice(seq);
          if (other_slice != null) {
            SeqIndexNonEqual sine = SeqIndexNonEqual.find(other_slice);
            if ((sine != null) && sine.enoughSamples()) {
              // System.out.println("Didn't realize SeqIndexNonEqual would be justified before NonEqual existed.");
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  // // Temporary, for debugging
  // public void destroy() {
  //   if (ppt.debugged) {
  //     System.out.println("NonEqual.destroy(" + ppt.name + ")");
  //     System.out.println(repr());
  //     (new Error()).printStackTrace();
  //   }
  //   super.destroy();
  // }

}
