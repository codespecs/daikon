package daikon;

import java.util.Vector;
import utilMDE.Assert;

/**
 * A VarComparabilityImplicit is an arbitrary integer, and comparisons
 * succeed exactly if the two integers are equal, except that negative
 * integers compare equal to everything.  Alternately, for an array
 * variable, a VarComparabilityImplicit may separately indicate
 * comparabilities for the elements and indices.
 * <pre>
 * VarComparabilityImplicit ::= int
 *                            | VarComparabilityImplicit "[" int "]"
 * </pre>
 * <p>
 *
 * This is called "implicit" because the comparability objects do not refer
 * to one another or refer directly to variables; whether two variables are
 * comparable depends on their comparability objects.  Implicit
 * comparability has the flavor of types in programming languages.<p>
 *
 * Soon, this will probably be modified to permit the group identifiers to
 * be arbitrary strings (not containing square brackets) instead of
 * arbitrary integers.
 **/
public final class VarComparabilityImplicit extends VarComparability implements java.io.Serializable {

  int base;
  VarComparabilityImplicit[] indexTypes; // indexTypes[0] is comparability of
                                // the first index of this array.
  int dimensions;		// Indicates how many of the indices are in use;
				// there may be more indices than this.

  private VarComparabilityImplicit cached_element_type;

  private static VarComparabilityImplicit unknown = new VarComparabilityImplicit(-3, null, 0);

  private VarComparabilityImplicit(int base, VarComparabilityImplicit[] indexTypes, int dimensions) {
    this.base = base;
    this.indexTypes = indexTypes;
    this.dimensions = dimensions;
  }

  static VarComparabilityImplicit parse(String rep, ProglangType vartype) {
    String rep_ = rep;          // for debugging

    Vector dim_reps = new Vector();
    while (rep.endsWith("]")) {
      int openpos = rep.lastIndexOf("[");
      dim_reps.add(0, rep.substring(openpos+1, rep.length()-1));
      rep = rep.substring(0, openpos);
    }
    int dims = dim_reps.size();
    VarComparabilityImplicit[] index_types = new VarComparabilityImplicit[dims];
    for (int i=0; i<dims; i++) {
      index_types[i] = parse((String)dim_reps.elementAt(i), null);
    }
    try {
      int base = Integer.parseInt(rep);
      return new VarComparabilityImplicit(base, index_types, dims);
    } catch (NumberFormatException e) {
      throw new Error(e.toString());
    }
  }

  public VarComparability makeAlias(VarInfoName name) {
    return this;
  }

  public VarComparability elementType() {
    if (cached_element_type == null) {
      // When Ajax is modified to output non-atomic info for arrays, this
      // check will no longer be necessary.
      if (dimensions > 0) {
        cached_element_type = new VarComparabilityImplicit(base, indexTypes, dimensions-1);
      } else {
        cached_element_type = unknown;
      }
    }
    return cached_element_type;
  }

  public VarComparability indexType(int dim) {
    // When Ajax is modified to output non-atomic info for arrays, this
    // check will no longer be necessary.
    if (dim < dimensions)
      return indexTypes[dim];
    else
      return unknown;
  }

  static boolean compatible(VarInfoName name1, VarComparabilityImplicit type1,
			    VarInfoName name2, VarComparabilityImplicit type2) {
    if ((type1.dimensions == 0) && (type1.base < 0))
      return true;
    if ((type2.dimensions == 0) && (type2.base < 0))
      return true;
    if ((type1.dimensions > 0) && (type2.dimensions > 0)) {
      return (compatible(type1.indexType(type1.dimensions-1),
                         type2.indexType(type2.dimensions-1))
              && compatible(type1.elementType(),
                            type2.elementType()));
    }
    if ((type1.dimensions == 0) && (type2.dimensions == 0))
      return type1.base == type2.base;
    // One array, one non-array, and the non-array isn't universally compatible.
    Assert.assert(type1.dimensions == 0 || type2.dimensions == 0);
    return false;
  }

  // for debugging
  public String toString() {
    String result = "" + base;
    for (int i=0; i<dimensions; i++) {
      result += "[" + indexType(i) + "]";
    }
    return result;
  }

}
