package daikon;

/**
 * A VarComparabilityImplicit is an arbitrary integer, and comparisons
 * succeed exactly if the two integers are equal, except that negative
 * integers compare equal to everything.  This is called "implicit" because
 * the comparability objects do not refer to one another or refer directly
 * to variables; whether two variables are comparable depends on their
 * comparability objects.  Implicit comparability has the flavor of types
 * in programming languages.<p>
 *
 * It would be nice to permit this to take arbitrary strings instead of
 * arbitrary integers.
 **/
public final class VarComparabilityImplicit extends VarComparability implements java.io.Serializable {
  private int index;

  private VarComparabilityImplicit(int index) { this.index = index; }

  private static VarComparabilityImplicit unknown = new VarComparabilityImplicit(-1);

  static VarComparabilityImplicit parse(String rep, ProglangType vartype) {
    try {
      int i = Integer.parseInt(rep);
      if (i < 0) {
	return unknown;
      } else {
	return new VarComparabilityImplicit(i);
      }
    } catch (NumberFormatException e) {
      return unknown;
    }
  }

  public VarComparability makeAlias(VarInfoName name) {
    return this;
  }

  public VarComparability elementType() {
    return unknown;
  }

  public VarComparability indexType(int dim) {
    return unknown;
  }

  static boolean compatible(VarInfoName name1, VarComparabilityImplicit type1,
			    VarInfoName name2, VarComparabilityImplicit type2) {
    return type1.index < 0 || type2.index < 0 || type1.index == type2.index;
  }
}
