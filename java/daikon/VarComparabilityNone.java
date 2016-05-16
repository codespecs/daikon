package daikon;

import java.io.Serializable;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Used when no VarComparability information is available (in the .dtrace file).
 * Every variable is considered comparable to every other variable.
 **/
public final class VarComparabilityNone extends VarComparability implements Serializable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // There is only one VarComparabilityNone object.
  public static final VarComparabilityNone it = new VarComparabilityNone();

  private VarComparabilityNone() {}

  static VarComparabilityNone parse(String rep, ProglangType vartype) {
    return it;
  }

  public VarComparability makeAlias() {
    return it;
  }

  public VarComparability elementType() {
    return it;
  }

  public VarComparability indexType(int dim) {
    return it;
  }

  public VarComparability string_length_type() {
    return it;
  }

  /*@Pure*/ public int hashCode() {
    return 0;
  }

  public boolean alwaysComparable() {
    return true;
  }

  /**
   * The best we can do without comparability info is to check if the
   * representation types in the data trace file are the same.  This
   * lets us compare integers to longs, but not integers to arrays.
   **/
  static /*@Pure*/ boolean comparable(VarComparabilityNone vcomp1, VarComparabilityNone vcomp2) {
    return true;
  }

  /*@SideEffectFree*/ public String toString() {
    return "no-comparability";
  }
}
