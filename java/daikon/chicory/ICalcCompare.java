package daikon.chicory;

/**
 * Classes implementing this interface have a callback method to calculate a comparability value.
 */
public interface ICalcCompare {
  /**
   * callback method to calculate a comparability value.
   *
   * @param dv variable to calculate comparability for
   * @param compare_ppt matching ppt from DynComp input; or null if none
   * @return string containing comparability value
   */
  public String calc_comparability(DaikonVariableInfo dv, DeclReader.DeclPpt compare_ppt);
}
