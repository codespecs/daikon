package daikon.chicory;

/**
 * Classes implementing this interface have a callback method to calculate a comparability value.
 */
public interface ICalcCompare {
  /** callback method to calculate a comparability value. */
  public String calc_comparability(DaikonVariableInfo dv, DeclReader.DeclPpt compare_ppt);
}
