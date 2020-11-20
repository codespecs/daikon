package daikon.chicory;

import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * Classes implementing this interface have a callback method to calculate a comparability value.
 */
public interface IComparability {
  /**
   * Calculate a comparability value.
   *
   * @param dv variable to calculate comparability for
   * @param compare_ppt corresponding ppt from DynComp input; or null if none
   * @return comparability value
   */
  public String getComparability(DaikonVariableInfo dv, DeclReader.@Nullable DeclPpt compare_ppt);
}
