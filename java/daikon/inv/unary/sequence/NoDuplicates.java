package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;



public class NoDuplicates extends SingleSequence {

  final static boolean debugNoDuplicates = false;

  int elts = 0;

  protected NoDuplicates(PptSlice ppt) {
    super(ppt);
  }

  public static NoDuplicates instantiate(PptSlice ppt) {
    return new NoDuplicates(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "NoDuplicates(" + var().name + "): "
      + "elts=\"" + elts
      + "; probability = " + probability;
  }

  public String format() {
    if (debugNoDuplicates) {
      System.out.println(repr());
    }
    return (var().name + " contains no duplicates");
  }


  public void add_modified(long[] a, int count) {
    for (int i=1; i<a.length; i++) {
      if (ArraysMDE.indexOf(a, a[i]) < i) {
        destroy();
        return;
      }
    }
    elts += a.length-1;
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      return Math.pow(.9, elts);
    }
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof NoDuplicates);
    return true;
  }
}
