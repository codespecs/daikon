package daikon;




// Internally, we use the names "array[]", "array[]-element", and
// "array[]-indexn".  These may be different depending on the programming
// language; for instance, C uses "*array" in place of "array[]-element".


public class VarComparability {

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
  // Setting this is temporary; eventually we will read it from the decl file.
  static int format = EXPLICIT;

}

