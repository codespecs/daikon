package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.Invariant;
import daikon.derive.unary.SequenceLength;
import java.util.*;
import utilMDE.*;

public class LinearBinary extends TwoScalar {

  public LinearBinaryCore core;

  protected LinearBinary(PptSlice ppt) {
    super(ppt);
    core = new LinearBinaryCore(this);
  }

  public static LinearBinary instantiate(PptSlice ppt) {
    return new LinearBinary(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "LinearBinary" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + ",probability = " + probability
      + "; " + core.repr();
  }

  public String format() {
    return core.format(var1().name, var2().name);
  }

  public String format_esc() {
    return core.format(var1().esc_name(), var2().esc_name());
  }

  public void add_modified(long x, long y, int count) {
    core.add_modified(x, y, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
  }

  public boolean isObviousDerived() {
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    // avoid "size(a)-1 = size(a) - 1"
    if (var1.isDerived() && (var1.derived instanceof SequenceLength)
        && var2.isDerived() && (var2.derived instanceof SequenceLength)) {
      SequenceLength sl1 = (SequenceLength) var1.derived;
      SequenceLength sl2 = (SequenceLength) var2.derived;
      if (sl1.base == sl2.base) {
        return true;
      }
    }
    return false;
  }


  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((LinearBinary) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof LinearBinary) {
      return core.isExclusiveFormula(((LinearBinary) other).core);
    }
    return false;
  }


  // Look up a previously instantiated invariant.
  public static LinearBinary find(PptSlice ppt) {
    Assert.assert(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof LinearBinary)
        return (LinearBinary) inv;
    }
    return null;
  }


}
