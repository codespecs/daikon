package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.Invariant;
import daikon.derive.unary.SequenceLength;
import java.util.*;
import utilMDE.*;

public class LinearTernary extends ThreeScalar {

  public final static boolean debugLinearTernary = false;
  // public final static boolean debugLinearTernary = true;

  public LinearTernaryCore core;

  protected LinearTernary(PptSlice ppt) {
    super(ppt);
    core = new LinearTernaryCore(this);
  }

  public static LinearTernary instantiate(PptSlice ppt) {
    LinearTernary result = new LinearTernary(ppt);
    if (debugLinearTernary) {
      System.out.println("LinearTernary.instantiate: " + result.repr());
    }
    return result;
  }

  public String repr() {
    return "LinearTernary" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; " + core.repr();
  }

  public String format() {
    return core.format(var1().name.name(), var2().name.name(), var3().name.name());
  }

  public String format_esc() {
    return core.format(var1().name.esc_name(), var2().name.esc_name(), var3().name.esc_name());
  }

  // public String format_reversed() {
  //   return core.format_reversed(var1().name.name(), var2().name.name(), var3().name.name());
  // }
  //
  // public String format_esc_reversed() {
  //   return core.format_reversed(var1().name.esc_name(), var2().name.esc_name(), var3().name.esc_name());
  // }

  public String format_simplify() {
    return "format_simplify " + this.getClass() + " needs to be changed: " + format();
  }

  public void add_modified(long x, long y, long z, int count) {
    core.add_modified(x, y, z, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
  }

  public boolean isObviousDerived() {
    // VarInfo var1 = ppt.var_infos[0];
    // VarInfo var2 = ppt.var_infos[1];
    // VarInfo var3 = ppt.var_infos[2];

    return false;
  }


  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((LinearTernary) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof LinearTernary) {
      return core.isExclusiveFormula(((LinearTernary) other).core);
    }
    return false;
  }


  // Look up a previously instantiated invariant.
  public static LinearTernary find(PptSlice ppt) {
    Assert.assert(ppt.arity == 3);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof LinearTernary)
        return (LinearTernary) inv;
    }
    return null;
  }

  // Returns a vector of LinearTernary objects.
  // This ought to produce an iterator instead.
  public static Vector findAll(VarInfo vi) {
    Vector result = new Vector();
    for (Iterator itor = vi.ppt.views_iterator() ; itor.hasNext() ; ) {
      PptSlice view = (PptSlice) itor.next();
      if ((view.arity == 3) && view.usesVar(vi)) {
        LinearTernary lt = LinearTernary.find(view);
        if (lt != null) {
          result.add(lt);
        }
      }
    }
    return result;
  }

}
