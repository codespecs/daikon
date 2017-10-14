package daikon;

import java.io.Serializable;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Used when no VarComparability information is available (in the {@code .dtrace} file). Every
 * variable is considered comparable to every other variable.
 */
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

  @Override
  public VarComparability makeAlias() {
    return it;
  }

  @Override
  public VarComparability elementType(/*>>>@GuardSatisfied VarComparabilityNone this*/) {
    return it;
  }

  @Override
  public VarComparability indexType(/*>>>@GuardSatisfied VarComparabilityNone this,*/ int dim) {
    return it;
  }

  @Override
  public VarComparability string_length_type() {
    return it;
  }

  /*@Pure*/
  @Override
  public int hashCode(/*>>>@GuardSatisfied VarComparabilityNone this*/) {
    return 0;
  }

  @Override
  public boolean alwaysComparable(/*>>>@GuardSatisfied VarComparabilityNone this*/) {
    return true;
  }

  /**
   * The best we can do without comparability info is to check if the representation types in the
   * data trace file are the same. This lets us compare integers to longs, but not integers to
   * arrays.
   */
  /*@Pure*/
  static boolean comparable(
      /*@GuardSatisfied*/ VarComparabilityNone vcomp1,
      /*@GuardSatisfied*/ VarComparabilityNone vcomp2) {
    return true;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied VarComparabilityNone this*/) {
    return "no-comparability";
  }
}
