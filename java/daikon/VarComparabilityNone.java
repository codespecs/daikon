package daikon;

import java.util.*;

import utilMDE.*;

public class VarComparabilityNone extends VarComparability {

  // There is only one VarComparabilityNone object.
  static VarComparabilityNone it = new VarComparabilityNone();

  private VarComparabilityNone() { };

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
