package daikon.inv.unary.sequence;

import daikon.*;
import daikon.derive.unary.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.*;
import java.util.*;
import utilMDE.*;

// This class represnts a comparison between elements of a sequence
// and the indices of those elements; for instance,
//   "for all i, a[i] > i".
public final class SeqIndexComparison extends SingleSequence {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;

  public IntComparisonCore core;

  static boolean debugSeqIndexComparison = false;

  protected SeqIndexComparison(PptSlice ppt) {
    super(ppt);
    Assert.assert(var().rep_type == ProglangType.INT_ARRAY);
    core = new IntComparisonCore(this);
    if (debugSeqIndexComparison) {
      System.out.println("Instantiated: " + format());
    }
  }

  public static SeqIndexComparison instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;

    VarInfo seqvar = ppt.var_infos[0];

    // if (isEqualToObviousSeqIndexComparison(sclvar, seqvar)) {
    //   Global.implied_noninstantiated_invariants += 1;
    //   if (debugSeqIndexComparison) {
    //     System.out.println("SeqIndexComparison not instantiated (obvious): "
    //                        + sclvar.name + " in " + seqvar.name);
    //   }
    //   return null;
    // }

    if (debugSeqIndexComparison) {
      System.out.println("SeqIndexComparison instantiated: " + seqvar.name);
    }

    // Don't compare indices to object addresses.
    ProglangType elt_type = seqvar.type.elementType();
    if (! elt_type.baseIsIntegral()) {
      return null;
    }
    VarComparability elt_compar = seqvar.comparability.elementType();
    VarComparability index_compar = seqvar.comparability.indexType(0);
    if (! VarComparability.comparable(VarInfoName.parse("seqvar.name.elementName"), elt_compar,
                                      VarInfoName.parse("seqvar.name.indexName0"), index_compar)) {
      return null;
    }

    return new SeqIndexComparison(ppt);
  }

  // public boolean isObviousImplied() {
  //   return isEqualToObviousSeqIndexComparison(sclvar(), seqvar());
  // }

  public String repr() {
    return "SeqIndexComparison" + varNames() + ": "
      + core.repr()
      + ",no_invariant=" + no_invariant;
  }

  public String format() {
    String comparator = core.format_comparator();
    // this is wrong because "a[i:k..] < i" doesn't need the subscript
    // return var().name.applySubscript(VarInfoName.parse("i")).name() + " " + comparator + " i";
    VarInfoName name = var().name;
    if ((new VarInfoName.ElementsFinder(name)).elems() != null) {
      return name.applySubscript(VarInfoName.parse("i")).name() + " " + comparator + " i";
    } else {
      return name.name() + " " + comparator + " (index)";
    }
  }

  /* IOA */
  public String format_ioa(String classname) {
    if (var().isIOASet())
      return "Not valid for Sets: " + format();
    String comparator = (core.format_comparator().equals("==")) ?
	"=" : core.format_comparator();
    String[] form =
      VarInfoName.QuantHelper.format_ioa(new VarInfo[] { var() }, classname);
    return form[0] + form[1] + " " + comparator + " " + form[3] + form[2];
  }

  public String format_esc() {
    String comparator = core.format_comparator();
    String[] form =
      VarInfoName.QuantHelper.format_esc(new VarInfoName[]
	{ var().name });
    return form[0] + "(" + form[1] + " " + comparator + " i)" + form[2];
  }

  public String format_simplify() {
    String comparator = core.format_comparator();
    if ("==".equals(comparator)) {
      comparator = "EQ";
    }
    String[] form =
      VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
	{ var().name });
    return form[0] + "(" + comparator + " " + form[1] + " |i|)" + form[2];
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
    return core.isSameFormula(((SeqIndexComparison) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    return false;
  }

  // Look up a previously instantiated invariant.
  public static SeqIndexComparison find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof SeqIndexComparison)
        return (SeqIndexComparison) inv;
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


  // Copied from IntComparison.
  public boolean isObviousImplied() {
    if (isExact()) {
      return false;
    }
    VarInfo seqvar = var();

    return false;
  }

}
