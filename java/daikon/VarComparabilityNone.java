package daikon;

import java.io.Serializable;

/**
 * Used when no VarComparability information is available (in the .dtrace file).
 * Every variable is considered comparable to every other variable.
 **/
public final class VarComparabilityNone
  extends VarComparability
  implements Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // There is only one VarComparabilityNone object.
  static VarComparabilityNone it = new VarComparabilityNone();

  private VarComparabilityNone() { }

  static VarComparabilityNone parse(String rep, ProglangType vartype) {
    return it;
  }

  public VarComparability makeAlias(VarInfoName name) {
    return it;
  }

  public VarComparability elementType() {
    return it;
  }

  public VarComparability indexType(int dim) {
    return it;
  }

  static boolean comparable(VarInfoName name1, VarComparabilityNone type1,
			    VarInfoName name2, VarComparabilityNone type2) {
    return true;
  }
}
