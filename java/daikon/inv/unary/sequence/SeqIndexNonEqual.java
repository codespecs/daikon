package daikon.inv.unary.sequence;

import daikon.*;
import daikon.derive.unary.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoSequence.*;
import daikon.inv.*;
import java.util.*;
import utilMDE.*;

public final class SeqIndexNonEqual extends SingleSequence {

  public NonEqualCore core;

  static boolean debugSeqIndexNonEqual = false;

  protected SeqIndexNonEqual(PptSlice ppt) {
    super(ppt);

    VarInfo var = var();
    Assert.assert(var.rep_type == ProglangType.INT_ARRAY);
    Assert.assert(var.type.elementType().isIntegral());
    core = new NonEqualCore(this, 0);

    if (debugSeqIndexNonEqual) {
      System.out.println("Instantiated: " + format());
    }
  }

  public static SeqIndexNonEqual instantiate(PptSlice ppt) {
    VarInfo seqvar = ppt.var_infos[0];

    if (debugSeqIndexNonEqual) {
      System.out.println("SeqIndexNonEqual instantiated: " + seqvar.name);
    }

    // Don't compare indices to object addresses.
    ProglangType elt_type = seqvar.type.elementType();
    if (! elt_type.baseIsIntegral())
      return null;

    return new SeqIndexNonEqual(ppt);
  }

  public String repr() {
    return "SeqIndexNonEqual" + varNames() + ": "
      + core.repr()
      + ",no_invariant=" + no_invariant;
  }

  public String format() {
    // Don't do this; format() shouldn't check justification (in general...).
    // Assert.assert(!justified() || can_be_eq || can_be_lt || can_be_gt);
    String array_base = var().name;
    if (array_base.endsWith("[]"))
      array_base = array_base.substring(0, array_base.length() - 2);
    return array_base + "[i] != i";
  }

  public String format_esc() {
    String[] esc_forall = var().esc_forall();
    return "(" + esc_forall[0]
      + "(" + esc_forall[1] + " != i))";
  }

  public void add_modified(long [] a, int count) {
    for (int i=0; i<a.length; i++) {
      core.add_modified(a[i], i, count);
      if (no_invariant)
        return;
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return core.isExact();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((SeqIndexNonEqual) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof SeqIndexComparison) {
      if (((SeqIndexComparison)other).isExact()) {
        return true;
      }
    }
    return false;
  }

  // Look up a previously instantiated invariant.
  public static SeqIndexNonEqual find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof SeqIndexNonEqual)
        return (SeqIndexNonEqual) inv;
    }
    return null;
  }



  // Copied from IntComparison.
  // public boolean isExclusiveFormula(Invariant other)
  // {
  //   if (other instanceof IntComparison) {
  //     return core.isExclusiveFormula(((IntComparison) other).core);
  //   }
  //   if (other instanceof NonEqual) {
  //     return isExact();
  //   }
  //   return false;
  // }


  // public boolean isObviousImplied() {
  //   return isEqualToObviousSeqIndexNonEqual(sclvar(), seqvar());
  // }


  // Copied from IntComparison.
  public boolean isObviousImplied() {
    VarInfo seqvar = var();

    // For each other sequence variable, if it is a supersequence of this
    // one and it has the same invariant, then this one is obvious.
    PptTopLevel pptt = (PptTopLevel) ppt.parent;
    for (int i=0; i<pptt.var_infos.length; i++) {
      VarInfo vi = pptt.var_infos[i];
      if (SubSequence.isObviousDerived(seqvar, vi)) {
        PptSlice1 other_slice = pptt.findSlice(vi);
        SeqIndexNonEqual other_sine = SeqIndexNonEqual.find(other_slice);
        if ((other_sine != null) && other_sine.justified()) {
          return true;
        }
      }
    }

    return false;
  }

}
