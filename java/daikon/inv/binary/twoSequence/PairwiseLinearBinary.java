package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;

public class PairwiseLinearBinary extends TwoSequence {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface
  public static boolean dkconfig_enabled = true;

  LinearBinaryCore core;

  protected PairwiseLinearBinary(PptSlice ppt) {
    super(ppt);
    core = new LinearBinaryCore(this);
  }

  public static PairwiseLinearBinary instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    return new PairwiseLinearBinary(ppt);
  }

  public String repr() {
    return "PairwiseLinearBinary" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; " + core.repr();
  }

  public String format() {
    return core.format(var1().name.name(), var2().name.name());
  }

  /* IOA */
  public String format_ioa(String classname) {
    if (var1().isIOASet() || var2().isIOASet()) 
      return "Not valid for sets: " + format();
    String[] form = 
      VarInfoName.QuantHelper.format_ioa(new VarInfo[] { var1(), var2() }, classname);
    return form[0]+"("+form[4]+"="+form[5]+") => ("+core.format_ioa(form[1], form[2])+")"+form[3];
  }
    
  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }

  public String format_simplify() {
    return "format_simplify " + this.getClass() + " needs to be changed: " + format();    
  }

  public void add_modified(long[] x_arr, long[] y_arr, int count) {
    if (x_arr.length != y_arr.length) {
      destroy();
      return;
    }
    int len = x_arr.length;
    // int len = Math.min(x_arr.length, y_arr.length);

    for (int i=0; i<len; i++) {
      long x = x_arr[i];
      long y = y_arr[i];

      core.add_modified(x, y, count);
      if (no_invariant) {
        // destroy() must have already been called
        return;
      }
    }
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((PairwiseLinearBinary) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof PairwiseLinearBinary) {
      return core.isExclusiveFormula(((PairwiseLinearBinary) other).core);
    }
    return false;
  }

}
