package daikon;


// Internally, we use the names "array[]", "array[]-element", and
// "array[]-indexn".  These may be different depending on the programming
// language; for instance, C uses "*array" in place of "array[]-element".


/**
 * VarComparability types have three formats: implicit, explicit, and none.<p>
 *
 * A VarComparabilityImplicit is an arbitrary string, and comparisons
 * succeed exactly if the two VarComparabilitys are identical.<p>
 *
 * A VarComparabilityExplicit is a list of other variable names, and
 * comparisons succeed if each variable is in the list of the other.<p>
 *
 * VarComparabilityNone means no comparability information was provided.<p>
 **/
public abstract class VarComparability {

  final static int NONE = 0;
  final static int IMPLICIT = 1;
  final static int EXPLICIT = 2;

  static VarComparability parse(int format, String rep, ProglangType vartype) {
    if (format == NONE) {
      return VarComparabilityNone.parse(rep, vartype);
    } else if (format == IMPLICIT) {
      return VarComparabilityImplicit.parse(rep, vartype);
    } else if (format == EXPLICIT) {
      return VarComparabilityExplicit.parse(rep, vartype);
    } else {
      throw new Error("bad format argument " + format
                      + " should have been in {0, 1, 2}");
    }
  }

  public static VarComparability makeAlias(VarInfo vi) {
    return vi.comparability.makeAlias(vi.name);
  }
  public abstract VarComparability makeAlias(VarInfoName name);

  public abstract VarComparability elementType();
  public abstract VarComparability indexType(int dim);

  public static boolean compatible(VarInfo v1, VarInfo v2) {
    return compatible(v1.name, v1.comparability, v2.name, v2.comparability);
  }

  public static boolean compatible(VarInfoName name1, VarComparability type1,
                                   VarInfoName name2, VarComparability type2) {

    if (type1.getClass() != type2.getClass())
      throw new Error("Trying to compare VarComparabilities " +
                      "of different types: " + Global.lineSep
                      + "    " + name1 + " " + type1 + Global.lineSep
                      + "    " + name2 + " " + type2);

    if (type1 instanceof VarComparabilityNone) {
      return VarComparabilityNone.compatible
        (name1, (VarComparabilityNone)type1,
         name2, (VarComparabilityNone)type2);
    } else if (type1 instanceof VarComparabilityImplicit) {
	return VarComparabilityImplicit.compatible
          (name1, (VarComparabilityImplicit)type1,
           name2, (VarComparabilityImplicit)type2);
    } else if (type1 instanceof VarComparabilityExplicit) {
      return VarComparabilityExplicit.compatible
        (name1, (VarComparabilityExplicit)type1,
         name2, (VarComparabilityExplicit)type2);
    } else {
      throw new Error("Unrecognized subtype of VarComparability: " + type1);
    }
  }

}
