package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.Invariant;
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

  public void add_modified(long x, long y, int count) {
    core.add_modified(x, y, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
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
