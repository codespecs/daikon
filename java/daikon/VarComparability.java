package daikon;




// Internally, we use the names "array[]", "array[]-element", and
// "array[]-indexn".  These may be different depending on the programming
// language; for instance, C uses "*array" in place of "array[]-element".


public abstract class VarComparability {

  // VarComparability types have two formats.
  //  * An implicit VarComparability is an arbitrary string, and
  //    comparisons succeed exactly if the two VarComparabilitys are identical.
  //    For now, we don't support implicit VarComparabilitys.  However, tests
  //    over programming language types look like that (so I could set the
  //    VarComparabilitys to the programming language types and use the
  //    implicit format).
  //  * An explicit VarComparability is a list of other variables, and comparisons
  //    succeed if each variable is in the list of the other.


  final static int NONE = 0;
  final static int IMPLICIT = 1;
  final static int EXPLICIT = 2;
  // // Setting this is temporary; eventually we will read it from the decl file.
  // static int format = EXPLICIT;

  static VarComparability parse(int format, String rep, ProglangType vartype) {
    if (format == NONE) {
      // Should I return a dummy object that responds to VarComparability methods?
      return VarComparabilityNone.parse(rep, vartype);
    } else if (format == IMPLICIT) {
      throw new Error("Not yet implemented");
    } else if (format == EXPLICIT) {
      return VarComparabilityExplicit.parse(rep, vartype);
    } else {
      throw new Error("bad format argument " + format
                      + " should have been in {0, 1, 2}");
    }
  }

  // Can't make static methods abstract, so have it throw an error
  public static VarComparability makeAlias(VarInfo vi) {
    return vi.comparability.makeAlias(vi.name);
  }
  public abstract VarComparability makeAlias(String name);

  public abstract VarComparability elementType();
  public abstract VarComparability indexType(int dim);

  static boolean compatible(String name1, VarComparability type1,
                            String name2, VarComparability type2) {
    if (type1.getClass() != type2.getClass())
      throw new Error("Trying to compare VarComparabilities of different types");
    if (type1 instanceof VarComparabilityNone) {
      return VarComparabilityNone.compatible(name1, type1,
                                             name2, (VarComparabilityNone)type2);
    // This class doesn't even exist yet!
    // } else if (type1 instanceof VarComparabilityImplicit) {
    //   throw new Error("Not yet implemented");
    //   // return VarComparabilityNone.compatible(name1, (VarComparabilityNone)type1,
    //   //                                        name2, (VarComparabilityNone)type2);
    } else if (type1 instanceof VarComparabilityNone) {
      return VarComparabilityExplicit.compatible(name1, (VarComparabilityExplicit)type1,
                                             name2, (VarComparabilityExplicit)type2);
    } else {
      throw new Error("What type is this? " + type1);
    }
  }

}

