package daikon.inv.sequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

// States that the value is one of the specified values.

// This subsumes an "exact" invariant that says the value is always exactly
// a specific value.  Do I want to make that a separate invariant
// nonetheless?  Probably not, as this will simplify implication and such.

// Similar to OneOfScalar; if I change one, change the other.
public class OneOfSequence extends SingleSequence implements OneOf {
  static final int LIMIT = 5;	// maximum size for the one_of list
  // probably needs to keep its own list of the values

  private int[][] elts;
  private int num_elts;

  OneOfSequence(PptSlice ppt_) {
    super(ppt_);
    elts = new int[LIMIT][];
    num_elts = 0;
  }

  public static OneOfSequence instantiate(PptSlice ppt) {
    return new OneOfSequence(ppt);
  }

  public int num_elts() {
    return num_elts;
  }

  public Object elt() {
    if (num_elts != 1)
      throw new Error("Represents " + num_elts + " elements");
    return elts[0];
  }

  private String subarray_rep() {
    StringBuffer sb = new StringBuffer();
    sb.append("{ ");
    for (int i=0; i<num_elts; i++) {
      if (i != 0)
        sb.append(", ");
      sb.append(ArraysMDE.toString(elts[i]));
    }
    sb.append(" }");
    return sb.toString();
  }

  public String repr() {
    double probability = getProbability();
    return "OneOfSequence(" + var().name + "): "
      + "no_invariant=" + no_invariant
      + ", num_elts=" + num_elts
      + ", elts=" + subarray_rep();
  }

  public String format() {
    if (no_invariant || (num_elts == 0) || (! justified()))
      return null;
    if (num_elts == 1)
      return var().name + " = " + ArraysMDE.toString(elts[0]);
    else
      return var().name + " one of " + subarray_rep();
  }


  public void add_modified(int[] v, int count) {
    // I can test equality with == because the arrays have been interned.
    for (int i=0; i<num_elts; i++)
      if (elts[i] == v)
        return;
    if (num_elts == LIMIT) {
      destroy();
      return;
    }
    elts[num_elts] = v;
    num_elts++;
  }

  protected double computeProbability() {
    // This is wrong; fix it
    return 0;
  }

}
