package daikon.chicory;

import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * Classes implementing this interface have a callback method to calculate a comparability value.
 */
public interface ICalcCompare {
  /**
   * Callback method to calculate a comparability value.
   *
   * @param dv variable to calculate comparability for
   * @param compare_ppt matching ppt from DynComp input; or null if none
   * @return comparability value
   */
  public String calc_comparability(DaikonVariableInfo dv, DeclReader.@Nullable DeclPpt compare_ppt);
}
