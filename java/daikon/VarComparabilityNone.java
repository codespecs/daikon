package daikon;

/**
 * Used when no VarComparability information is available (in the .dtrace file).
 * Every variable is considered comparable to every other variable.
 **/
public final class VarComparabilityNone extends VarComparability implements java.io.Serializable {

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
