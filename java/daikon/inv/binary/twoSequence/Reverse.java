package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.Assert;

public class Reverse
  extends TwoSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff Reverse invariants should be considered.
   **/
  public static boolean dkconfig_enabled = true;

  protected Reverse(PptSlice ppt) {
    super(ppt);
  }

  public static Reverse instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    return new Reverse(ppt);
  }  

  protected Invariant resurrect_done_swapped() {
    // "reverse of" is symmetric
    return this;
  }

  public String repr() {
    return "Reverse" + varNames() + ": "
      + "falsified=" + falsified;
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.JAVA) return format_java();
    if (format == OutputFormat.IOA) return format_ioa();

    return format_unimplemented(format);
  }

  public String format_daikon() {
    return var1().name.name() + " is the reverse of " + var2().name.name();
  }

  public String format_java() {
    // ( (new StringBuffer (var1().name.name())).reverse().toString(
    //       ).equals (var2().name.name()))
    return "( (new StringBuffer (" + var1().name.name() + ")).reverse().toString().equals (" + var2().name.name() + ")";
  }

  /* IOA */
  public String format_ioa() {
    return "Not valid for Sets or Arrays: " + format();
  }

  public void add_modified(long[] a1, long[] a2, int count) {
    if (a1.length != a2.length) {
      flowThis();
      destroy();
      return;
    }
    int len = a1.length;
    for (int i=0, j=len-1; i<len; i++, j--)
      if (a1[i] != a2[j]) {
	flowThis();
        destroy();
        return;
      }
  }


  protected double computeProbability() {
    if (falsified)
      return Invariant.PROBABILITY_NEVER;
    else
      return Invariant.PROBABILITY_JUSTIFIED;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof Reverse);
    return true;
  }

}
