package daikon;

/**
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

  static boolean compatible(String name1_, VarComparabilityImplicit type1,
			    String name2_, VarComparabilityImplicit type2) {
    return type1.index < 0 || type2.index < 0 || type1.index == type2.index;
  }
}

