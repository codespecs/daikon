package daikon.inv.twoScalar;

import daikon.*;

class LinearBinary extends TwoScalar {

  LinearBinaryCore core;

  protected LinearBinary(PptSlice ppt_) {
    super(ppt_);
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
    if (no_invariant || ! justified()) {
      return null;
    }
    return core.format(var1().name, var2().name);
  }

  public void add_modified(int x, int y, int count) {
    core.add_modified(x, y, count);
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
  }

}
