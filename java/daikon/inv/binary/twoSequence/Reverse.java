package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.Assert;

public class Reverse extends TwoSequence {

  protected Reverse(PptSlice ppt) {
    super(ppt);
  }

  public static Reverse instantiate(PptSlice ppt) {
    return new Reverse(ppt);
  }


  public String repr() {
    double probability = getProbability();
    return "Reverse" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; probability = " + probability;
  }

  public String format() {
    return var1().name + " is the reverse of " + var2().name;
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
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
