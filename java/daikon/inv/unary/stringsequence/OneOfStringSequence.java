package daikon.inv.unary.stringsequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

// *****
// Automatically generated from OneOf-cpp.java
// *****

// States that the value is one of the specified values.

// This subsumes an "exact" invariant that says the value is always exactly
// a specific value.  Do I want to make that a separate invariant
// nonetheless?  Probably not, as this will simplify implication and such.

public final class OneOfStringSequence  extends SingleStringSequence  implements OneOf {
  final static int LIMIT = 5;	// maximum size for the one_of list
  // Probably needs to keep its own list of the values, and number of each seen.
  // (That depends on the slice; maybe not until the slice is cleared out.
  // But so few values is cheap, so this is quite fine for now and long-term.)

  private String[] [] elts;
  private int num_elts;

  OneOfStringSequence (PptSlice ppt) {
    super(ppt);

    elts = new String[LIMIT][];    // elements are interned, so can test with ==
                                // (in the general online case, not worth interning)

    num_elts = 0;

  }

  public static OneOfStringSequence  instantiate(PptSlice ppt) {
    return new OneOfStringSequence (ppt);
  }

  public int num_elts() {
    return num_elts;
  }

  public Object elt() {
    if (num_elts != 1)
      throw new Error("Represents " + num_elts + " elements");

    return elts[0];

  }

  static Comparator comparator = new ArraysMDE.ComparableArrayComparatorLexical();

  private void sort_rep()
  {
    Arrays.sort(elts, 0, num_elts , comparator );
  }

  private String subarray_rep() {
    // Not so efficient an implementation, but simple;
    // and how often will we need to print this anyway?
    sort_rep();
    StringBuffer sb = new StringBuffer();
    sb.append("{ ");
    for (int i=0; i<num_elts; i++) {
      if (i != 0)
        sb.append(", ");
      sb.append(ArraysMDE.toString( elts[i] ) );
    }
    sb.append(" }");
    return sb.toString();
  }

  public String repr() {
    double probability = getProbability();
    return "OneOfStringSequence(" + var().name + "): "
      + "no_invariant=" + no_invariant
      + ", num_elts=" + num_elts
      + ", elts=" + subarray_rep();
  }

  public String format() {
    if (num_elts == 1) {

      return var().name  + " = " + ArraysMDE.toString( elts[0] ) ;

    } else {
      return var().name  + " one of " + subarray_rep();
    }
  }

  public void add_modified(String[]  v, int count) {

    Assert.assert(Intern.isInterned(v));

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
    // This is not ideal.
    if (num_elts == 0) {
      return Invariant.PROBABILITY_UNKNOWN;

    } else {
      return Invariant.PROBABILITY_JUSTIFIED;
    }
  }

  public boolean isSameFormula(Invariant o)
  {
    OneOfStringSequence  other = (OneOfStringSequence ) o;
    if (num_elts != other.num_elts)
      return false;

    sort_rep();
    other.sort_rep();
    for (int i=0; i < num_elts; i++)
      if (elts[i] != other.elts[i]) // elements are interned
	return false;

    return true;
  }

  public boolean isExclusiveFormula(Invariant o)
  {
    if (o instanceof OneOfStringSequence ) {
      OneOfStringSequence  other = (OneOfStringSequence ) o;

      for (int i=0; i < num_elts; i++) {
        for (int j=0; j < other.num_elts; j++) {
          if (elts[i] == other.elts[j]) // elements are interned
            return false;
        }
      }
      return true;
    }
    // Many more checks can be added here:  against nonzero, modulus, etc.

    return false;
  }

  // Look up a previously instantiated invariant.
  public static OneOfStringSequence  find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof OneOfStringSequence )
        return (OneOfStringSequence ) inv;
    }
    return null;
  }

}
