package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.Invariant;
import java.lang.reflect.*;


public class FunctionUnary extends TwoScalar {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface
  public static boolean dkconfig_enabled = true;

  FunctionUnaryCore core;

  protected FunctionUnary(PptSlice ppt, String methodname, Method function, boolean inverse) {
    super(ppt);
    core = new FunctionUnaryCore(this, methodname, function, inverse);
  }

  public static FunctionUnary instantiate(PptSlice ppt, String methodname, Method function, boolean inverse) {
    if (!dkconfig_enabled) return null;
    return new FunctionUnary(ppt, methodname, function, inverse);
  }

  public String repr() {
    return "FunctionUnary" + varNames() + ": " + core.repr();
  }

  public String format() {
    // core.format takes VarInfoName objects, not Strings.  (Why?)
    return core.format(var1().name, var2().name);
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }

  /* IOA */
  public String format_ioa(String classname) {
    return core.format_ioa(var1().name.ioa_name(classname), var2().name.ioa_name(classname));
  }

  public String format_simplify() {
    return "format_simplify " + this.getClass() + " needs to be changed: " + format();
  }

  public void add_modified(long x_int, long y_int, int count) {
    core.add_modified(x_int, y_int, count);
  }


  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((FunctionUnary) other).core);
  }
}
