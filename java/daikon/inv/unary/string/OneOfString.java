package daikon.inv.unary.string;

import daikon.*;
import daikon.inv.*;
import daikon.derive.unary.*;
import daikon.inv.unary.sequence.*;

import utilMDE.*;

import java.util.*;

// *****
// Automatically generated from OneOf-cpp.java
// *****

// States that the value is one of the specified values.

// This subsumes an "exact" invariant that says the value is always exactly
// a specific value.  Do I want to make that a separate invariant
// nonetheless?  Probably not, as this will simplify implication and such.

public final class OneOfString  extends SingleString  implements OneOf {
  final static int LIMIT = 5;	// maximum size for the one_of list
  // Probably needs to keep its own list of the values, and number of each seen.
  // (That depends on the slice; maybe not until the slice is cleared out.
  // But so few values is cheap, so this is quite fine for now and long-term.)

  private String [] elts;
  private int num_elts;

  OneOfString (PptSlice ppt) {
    super(ppt);

    elts = new String [LIMIT];

    num_elts = 0;

  }

  public static OneOfString  instantiate(PptSlice ppt) {
    return new OneOfString (ppt);
  }

  public int num_elts() {
    return num_elts;
  }

  public Object elt() {
    if (num_elts != 1)
      throw new Error("Represents " + num_elts + " elements");

    return elts[0];

  }

  private void sort_rep() {
    Arrays.sort(elts, 0, num_elts  );
  }

  // Assumes the other array is already sorted
  public boolean compare_rep(int num_other_elts, String [] other_elts) {
    if (num_elts != num_other_elts)
      return false;
    sort_rep();
    for (int i=0; i < num_elts; i++)
      if (elts[i] != other_elts[i]) // elements are interned
        return false;
    return true;
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
      sb.append("\"" + UtilMDE.quote( elts[i] ) + "\"" );
    }
    sb.append(" }");
    return sb.toString();
  }

  public String repr() {
    double probability = getProbability();
    return "OneOfString(" + var().name + "): "
      + "no_invariant=" + no_invariant
      + ", num_elts=" + num_elts
      + ", elts=" + subarray_rep();
  }

  public String format() {
    String varname = var().name ;
    if (num_elts == 1) {

      return varname + " = \"" + UtilMDE.quote( elts[0] ) + "\"" ;

    } else {
      return varname + " one of " + subarray_rep();
    }
  }

  public String format_esc() {

    String varname = var().esc_name() ;

    String result = "";

    // Format   \typeof(theArray) = "[Ljava.lang.Object;"
    //   as     \typeof(theArray) == \type(java.lang.Object[])
    if (varname.startsWith("\\typeof")) {
      for (int i=0; i<num_elts; i++) {
        if (i>0) result += " || ";
        result += varname + " == " +
                 ((elts[i].equals("null"))
                  ? "\\typeof(null)"
                  : "\\type(" + UtilMDE.classnameFromJvm(elts[i]) + ")");
      }
    }

    return result;

  }

  public void add_modified(String  v, int count) {

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
    OneOfString  other = (OneOfString ) o;
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
    if (o instanceof OneOfString ) {
      OneOfString  other = (OneOfString ) o;

      for (int i=0; i < num_elts; i++) {
        for (int j=0; j < other.num_elts; j++) {
          if (elts[i] == other.elts[j]) // elements are interned
            return false;
        }
      }
      return true;
    }

    return false;
  }

  // Look up a previously instantiated invariant.
  public static OneOfString  find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof OneOfString )
        return (OneOfString ) inv;
    }
    return null;
  }

}
