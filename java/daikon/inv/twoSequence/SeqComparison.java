package daikon.inv.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

class SeqComparison extends TwoSequence implements Comparison {

  static Comparator comparator = new ArraysMDE.IntArrayComparatorLexical();

  final static boolean debugSeqComparison = false;

  boolean can_be_eq = false;
  boolean can_be_lt = false;
  boolean can_be_gt = false;

  protected SeqComparison(PptSlice ppt_) {
    super(ppt_);
  }

  public static SeqComparison instantiate(PptSlice ppt) {
    return new SeqComparison(ppt);
  }

  public String repr() {
    double probability = getProbability();
    return "SeqComparison(" + var1().name + "," + var2().name + "): "
      + "can_be_eq=" + can_be_eq
      + ",can_be_lt=" + can_be_lt
      + ",can_be_gt=" + can_be_gt
      + "; probability = " + probability;
  }

  public String format() {
    if (justified() && (can_be_eq || can_be_gt || can_be_lt)) {
      String inequality = (can_be_lt ? "<" : can_be_gt ? ">" : "");
      String comparison = (can_be_eq ? "=" : "");
      if (debugSeqComparison) {
        System.out.println(repr()
                           + "; inequality=\"" + inequality + "\""
                           + ",comparison=\"" + comparison + "\"");
      }
      return var1().name + " " + inequality + comparison + " " + var2().name;
    } else {
      return null;
    }
  }


  public void add_modified(int[] v1, int[] v2, int count) {
    int comparison = comparator.compare(v1, v2);
    // System.out.println("SeqComparison(" + var1().name + "," + var2().name + "): "
    //                    + "compare(" + ArraysMDE.toString(v1)
    //                    + ", " + ArraysMDE.toString(v2) + ") = " + comparison);
    if (comparison == 0)
      can_be_eq = true;
    else if (comparison < 0)
      can_be_lt = true;
    else
      can_be_gt = true;
    no_invariant = (can_be_lt && can_be_gt);
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      // I don't know how to compute a better probability for this.
      return 0;
    }
  }

  // For Comparison interface
  public double eq_probability() {
    if (can_be_eq && (!can_be_lt) && (!can_be_gt))
      return computeProbability();
    else
      return Invariant.PROBABILITY_NEVER;
  }

}
