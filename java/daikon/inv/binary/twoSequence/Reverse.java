package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.Assert;

public class Reverse extends TwoSequence {

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = true;

  protected Reverse(PptSlice ppt) {
    super(ppt);
  }

  public static Reverse instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    return new Reverse(ppt);
  }  

  protected Invariant resurrect_done(int[] permutation) {
    // "reverse of" is symmetric
    return this;
  }

  public String repr() {
    return "Reverse" + varNames() + ": "
      + "no_invariant=" + no_invariant;
  }

  public String format() {
    return var1().name.name() + " is the reverse of " + var2().name.name();
  }

  public String format_esc() {
    return "warning: method " + this.getClass() + ".format_esc() needs to be implemented: " + format();
  }

  /* IOA */
  public String format_ioa(String automaton) {
    return "Not valid for Sets or Arrays: " + format();
  }


  public String format_simplify() {
    return "warning: method " + this.getClass() + ".format_simplify() needs to be implemented: " + format();
  }

  public void add_modified(long[] a1, long[] a2, int count) {
    if (a1.length != a2.length) {
      destroy();
      return;
    }
    int len = a1.length;
    for (int i=0, j=len-1; i<len; i++, j--)
      if (a1[i] != a2[j]) {
        destroy();
        return;
      }
  }


  protected double computeProbability() {
    if (no_invariant)
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
