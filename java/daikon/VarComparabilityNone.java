package daikon;

/**
 * Used when no VarComparability information is available (in the .dtrace file).
 **/
public final class VarComparabilityNone extends VarComparability implements java.io.Serializable {

  // There is only one VarComparabilityNone object.
  static VarComparabilityNone it = new VarComparabilityNone();

  private VarComparabilityNone() { }

  static VarComparabilityNone parse(String rep, ProglangType vartype) {
    return it;
  }

  public VarComparability makeAlias(String name) {
    return it;
  }

  public VarComparability elementType() {
    return it;
  }

  public VarComparability indexType(int dim) {
    return it;
  }

}
